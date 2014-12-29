#include "opencv2/objdetect/objdetect.hpp"
#include "opencv2/highgui/highgui.hpp"
#include "opencv2/imgproc/imgproc.hpp"

#include <iostream>
#include <stdio.h>

using namespace std;
using namespace cv;

void detect( Mat& img,
             CascadeClassifier& cascade, CascadeClassifier& nestedCascade,
             double scale);
void showEyes();
void drawTracks(Mat& img);

String cascadeName = "haarcascade_frontalface_alt.xml";
String nestedCascadeName = "haarcascade_eye_tree_eyeglasses.xml";

const int maxEyes(8);
const cv::Size eyeSize(200, 150);

const Scalar colors[maxEyes] = {
        CV_RGB(0,0,255),
        CV_RGB(0,128,255),
        CV_RGB(0,255,255),
        CV_RGB(0,255,0),
        CV_RGB(255,128,0),
        CV_RGB(255,255,0),
        CV_RGB(255,0,0),
        CV_RGB(255,0,255)} ;


Mat eyes(cv::Size(eyeSize.width * 4, eyeSize.height * 2), CV_8UC3);

struct EyeTrack {
  vector<Rect> lastSeen;
  vector<Mat> frames;
  int numFramesSinceLastSeen;

  EyeTrack(): numFramesSinceLastSeen(0) {}
};

vector<EyeTrack> eyeTracks(maxEyes);

int main( int argc, const char** argv )
{
    CvCapture* capture = 0;
    Mat frame, frameCopy;
    const String scaleOpt = "--scale=";
    size_t scaleOptLen = scaleOpt.length();
    const String cascadeOpt = "--cascade=";
    size_t cascadeOptLen = cascadeOpt.length();
    const String nestedCascadeOpt = "--nested-cascade";
    size_t nestedCascadeOptLen = nestedCascadeOpt.length();
    String inputName;

    CascadeClassifier cascade, nestedCascade;
    double scale = 2;

    for( int i = 1; i < argc; i++ )
    {
        cout << "Processing " << i << " " <<  argv[i] << endl;
        if( cascadeOpt.compare( 0, cascadeOptLen, argv[i], cascadeOptLen ) == 0 )
        {
            cascadeName.assign( argv[i] + cascadeOptLen );
            cout << "  from which we have cascadeName= " << cascadeName << endl;
        }
        else if( nestedCascadeOpt.compare( 0, nestedCascadeOptLen, argv[i], nestedCascadeOptLen ) == 0 )
        {
            if( argv[i][nestedCascadeOpt.length()] == '=' )
                nestedCascadeName.assign( argv[i] + nestedCascadeOpt.length() + 1 );
            if( !nestedCascade.load( nestedCascadeName ) )
                cerr << "WARNING: Could not load classifier cascade for nested objects" << endl;
        }
        else if( scaleOpt.compare( 0, scaleOptLen, argv[i], scaleOptLen ) == 0 )
        {
            if( !sscanf( argv[i] + scaleOpt.length(), "%lf", &scale ) || scale < 1 )
                scale = 2;
            cout << " from which we read scale = " << scale << endl;
        }
        else if( argv[i][0] == '-' )
        {
            cerr << "WARNING: Unknown option %s" << argv[i] << endl;
        }
        else
            inputName.assign( argv[i] );
    }

    if( !cascade.load( cascadeName ) )
    {
        cerr << "ERROR: Could not load classifier cascade" << endl;
        return -1;
    }

    if( inputName.empty() || (isdigit(inputName.c_str()[0]) && inputName.c_str()[1] == '\0') )
    {
        capture = cvCaptureFromCAM( inputName.empty() ? 0 : inputName.c_str()[0] - '0' );
        int c = inputName.empty() ? 0 : inputName.c_str()[0] - '0' ;
        if(!capture) cout << "Capture from CAM " <<  c << " didn't work" << endl;
    }
    else if( inputName.size() )
    {
        capture = cvCaptureFromAVI( inputName.c_str() );
        if(!capture) cout << "Capture from AVI didn't work" << endl;
    }

    cvNamedWindow( "result", 1 );
    cv::namedWindow( "eyes", 0 );

    if( capture )
    {
        cout << "In capture ..." << endl;
        while(true)
        {
            IplImage* iplImg = cvQueryFrame( capture );
            frame = iplImg;
            if( frame.empty() )
                break;
            if( iplImg->origin == IPL_ORIGIN_TL )
                frame.copyTo( frameCopy );
            else
                flip( frame, frameCopy, 0 );

            detect( frameCopy, cascade, nestedCascade, scale );
	    drawTracks(frameCopy);
	    cv::imshow( "result", frameCopy );
	    showEyes();

            if( waitKey( 10 ) >= 0 )
                goto _cleanup_;
        }

        waitKey(0);

_cleanup_:
        cvReleaseCapture( &capture );
    }

    cv::destroyAllWindows();

    return 0;
}

void showEyes() {
  for (int i = 0; i < eyeTracks.size(); ++i) {
    const EyeTrack& eyeTrack = eyeTracks[i];
    if (!eyeTrack.frames.empty()) {
      eyeTrack.frames.back().copyTo(
        eyes(Rect(Point(eyeSize.width * (i % 4),
                        eyeSize.height * (i / 4)),
                  eyeSize))
      );
    }
  }
  cv::imshow( "eyes", eyes );
}

void drawTracks(Mat& img) {
  for (int i = 0; i < eyeTracks.size(); ++i) {
    const EyeTrack& eyeTrack = eyeTracks[i];
    for (int j = 0; j < eyeTrack.lastSeen.size(); ++j) {
      rectangle(img, eyeTrack.lastSeen[j], colors[i]);
    }
  }
}

std::ostream& operator<< (std::ostream& o, const cv::Size& s) {
  o << '(' << s.width << ", " << s.height << ')';
  return o;
}

template<typename Tp_> Rect_<Tp_> operator*(const Rect_<Tp_>& r, double scale) {
  return Rect_<Tp_>(r.x*scale, r.y*scale, r.width*scale, r.height*scale);
}

void detect( Mat& img,
             CascadeClassifier& cascade, CascadeClassifier& nestedCascade,
             double scale)
{
    int i = 0;
    double t = 0;
    vector<Rect> faces;
    Mat gray, smallImg( cvRound (img.rows/scale), cvRound(img.cols/scale), CV_8UC1 );

    cvtColor( img, gray, CV_BGR2GRAY );
    resize( gray, smallImg, smallImg.size(), 0, 0, INTER_LINEAR );
    equalizeHist( smallImg, smallImg );

    t = (double)cvGetTickCount();
    cascade.detectMultiScale( smallImg, faces,
        1.1, 2, 0
        //|CV_HAAR_FIND_BIGGEST_OBJECT
        //|CV_HAAR_DO_ROUGH_SEARCH
        |CV_HAAR_SCALE_IMAGE
        ,
        Size(30, 30) );
    t = (double)cvGetTickCount() - t;
    printf( "detection time = %g ms\n", t/((double)cvGetTickFrequency()*1000.) );
    for( vector<Rect>::const_iterator r = faces.begin(); r != faces.end(); r++, i++ )
    {
        Mat smallImgROI;
        vector<Rect> nestedObjects;
        Point center;
        Scalar color = colors[i%8];
        int radius;
        center.x = cvRound((r->x + r->width*0.5)*scale);
        center.y = cvRound((r->y + r->height*0.5)*scale);
        radius = cvRound((r->width + r->height)*0.25*scale);
        if( nestedCascade.empty() )
            continue;
        smallImgROI = smallImg(*r);
        nestedCascade.detectMultiScale( smallImgROI, nestedObjects,
            1.1, 2, 0
            //|CV_HAAR_FIND_BIGGEST_OBJECT
            //|CV_HAAR_DO_ROUGH_SEARCH
            //|CV_HAAR_DO_CANNY_PRUNING
            |CV_HAAR_SCALE_IMAGE
            ,
            Size(30, 30) );

	for (int i = 0; i < nestedObjects.size(); ++i) {
	  if (!(i < eyeTracks.size())) break;

	  const Rect& nr = nestedObjects[i];
	  Rect imgRect((nr + r->tl()) * scale);

	  eyeTracks[i].frames.push_back(Mat());
	  resize(img(imgRect), eyeTracks[i].frames.back(), eyeSize, 0, 0, INTER_LINEAR);

	  eyeTracks[i].lastSeen.push_back(imgRect);
	}

        for( vector<Rect>::const_iterator nr = nestedObjects.begin(); nr != nestedObjects.end(); nr++ )
        {
	  rectangle(img, (*nr + r->tl())*scale, color, 3);
        }
        rectangle( img, *r * scale, color, 3);
    }
}
