#include <iostream>
#include <stdio.h>

#include "opencv2/objdetect/objdetect.hpp"
#include "opencv2/highgui/highgui.hpp"
#include "opencv2/imgproc/imgproc.hpp"

#include "eyes.hpp"

using namespace std;
using namespace cv;

String cascadeName = "haarcascade_frontalface_alt.xml";
String nestedCascadeName = "haarcascade_eye_tree_eyeglasses.xml";

int main(int argc, const char** argv) {
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

  for (int i = 1; i < argc; i++) {
    cout << "Processing " << i << " " << argv[i] << endl;
    if (cascadeOpt.compare(0, cascadeOptLen, argv[i], cascadeOptLen) == 0) {
      cascadeName.assign(argv[i] + cascadeOptLen);
      cout << "  from which we have cascadeName= " << cascadeName << endl;
    } else if (nestedCascadeOpt.compare(0, nestedCascadeOptLen, argv[i],
        nestedCascadeOptLen) == 0) {
      if (argv[i][nestedCascadeOpt.length()] == '=')
        nestedCascadeName.assign(argv[i] + nestedCascadeOpt.length() + 1);
      if (!nestedCascade.load(nestedCascadeName))
        cerr << "WARNING: Could not load classifier cascade for nested objects"
            << endl;
    } else if (scaleOpt.compare(0, scaleOptLen, argv[i], scaleOptLen) == 0) {
      if (!sscanf(argv[i] + scaleOpt.length(), "%lf", &scale) || scale < 1)
        scale = 2;
      cout << " from which we read scale = " << scale << endl;
    } else if (argv[i][0] == '-') {
      cerr << "WARNING: Unknown option %s" << argv[i] << endl;
    } else
      inputName.assign(argv[i]);
  }

  if (!cascade.load(cascadeName)) {
    cerr << "ERROR: Could not load classifier cascade" << endl;
    return -1;
  }

  if (inputName.empty()
      || (isdigit(inputName.c_str()[0]) && inputName.c_str()[1] == '\0')) {
    capture = cvCaptureFromCAM(
        inputName.empty() ? 0 : inputName.c_str()[0] - '0');
    int c = inputName.empty() ? 0 : inputName.c_str()[0] - '0';
    if (!capture)
      cout << "Capture from CAM " << c << " didn't work" << endl;
  } else if (inputName.size()) {
    capture = cvCaptureFromAVI(inputName.c_str());
    if (!capture)
      cout << "Capture from AVI didn't work" << endl;
  }

  cvNamedWindow("result", 1);
  cv::namedWindow("eyes", 0);

  if (capture) {
    cout << "In capture ..." << endl;
    while (true) {
      IplImage* iplImg = cvQueryFrame(capture);
      frame = iplImg;
      if (frame.empty())
        break;
      if (iplImg->origin == IPL_ORIGIN_TL)
        frame.copyTo(frameCopy);
      else
        flip(frame, frameCopy, 0);

      detect(frameCopy, cascade, nestedCascade, scale);
      drawTracks(frameCopy);
      cv::imshow("result", frameCopy);
      showEyes();

      if (waitKey(10) >= 0)
        goto _cleanup_;
    }

    waitKey(0);

    _cleanup_: cvReleaseCapture(&capture);
  }

  cv::destroyAllWindows();

  return 0;
}

