#include "eyes.hpp"

#include <algorithm>
#include <vector>

#include <iostream>

#include "opencv2/objdetect/objdetect.hpp"
#include "opencv2/highgui/highgui.hpp"
#include "opencv2/imgproc/imgproc.hpp"
#include "opencv2/core.hpp"

using namespace std;
using namespace cv;

struct Settings {
  int maxEyeDistanceMovedPerFrame;  // In widths of an eye
} settings = {
  2
};

const int maxEyes(4);
const cv::Size eyeSize(200, 150);

const Scalar colors[] = {
    CV_RGB(0, 0, 255),
    CV_RGB(0, 128, 255),
    CV_RGB(0, 255, 255),
    CV_RGB(0, 255, 0),
    CV_RGB(255, 128, 0),
    CV_RGB(255, 255, 0),
    CV_RGB(255, 0, 0),
    CV_RGB(255, 0, 255)
};

Mat eyes(cv::Size(eyeSize.width * 4, eyeSize.height * 2), CV_8UC3);

vector<EyeTrack> eyeTracks(maxEyes);

Point center(const Rect& r) {
  return (r.tl() + r.br()) * 0.5;
}

void showEyes() {
  for (int i = 0; i < eyeTracks.size(); ++i) {
    const EyeTrack& eyeTrack = eyeTracks[i];
    Rect targetRect(Rect(Point(eyeSize.width * (i % 4), eyeSize.height * (i / 4)),
                         eyeSize));
    Mat target(eyes(targetRect));
    Point textPos(0, targetRect.height - 3);
    ostringstream s;
    s << eyeTrack.frames.size() << " / " << eyeTrack.numFramesSinceLastSeen;
    if (!eyeTrack.frames.empty()) {
      eyeTrack.frames.back().copyTo(target);
      putText(target, s.str(), textPos, FONT_HERSHEY_SIMPLEX, 0.8, 0, 2);
      putText(target, s.str(), textPos, FONT_HERSHEY_SIMPLEX, 0.8, colors[i], 1);
    } else {
      target.setTo(0);
      putText(target, "[...]", textPos, FONT_HERSHEY_SIMPLEX, 0.8, colors[i]);
    }
  }
  imshow("eyes", eyes);
}

void drawTracks(Mat& img) {
  for (int i = 0; i < eyeTracks.size(); ++i) {
    const EyeTrack& eyeTrack = eyeTracks[i];
    for (int j = 0; j < eyeTrack.lastSeen.size(); ++j) {
      rectangle(img, eyeTrack.lastSeen[j], colors[i]);
    }
  }
}

std::ostream& operator<<(std::ostream& o, const cv::Size& s) {
  o << '(' << s.width << ", " << s.height << ')';
  return o;
}

std::ostream& operator<<(std::ostream& o, const cv::Rect& r) {
  o << "Rect(" << r.tl() << ", " << r.size() << ')';
  return o;
}

template<typename Tp_> Rect_<Tp_> operator*(const Rect_<Tp_>& r, double scale) {
  return Rect_<Tp_>(r.x * scale, r.y * scale, r.width * scale, r.height * scale);
}

struct Match {
  Match(int i, int j) :
      i(i), j(j) {
  }
  bool operator()(const std::pair<double, std::pair<int, int> >& edge) const {
    return edge.second.first == i || edge.second.second == j;
  }
  int i, j;
};

vector<pair<int, int> > assignTracksToEyes(
    const vector<EyeTrack>& eyeTracks,
    const vector<Rect>& allEyes) {
  vector<pair<int, int> > result;

  // Build a list of distances from every active track to every eye observed this frame.
  int numActiveTracks = 0;
  vector<pair<double, pair<int, int> > > dists;
  for (int i = 0; i < eyeTracks.size(); ++i)
    if (!eyeTracks[i].lastSeen.empty()) {
      ++numActiveTracks;
      const EyeTrack& track = eyeTracks[i];
      for (int j = 0; j < allEyes.size(); ++j) {
        const Rect& lastSeen = track.lastSeen.back();
        double dist = norm(center(lastSeen) - center(allEyes[j]));
        if (dist <= lastSeen.width * settings.maxEyeDistanceMovedPerFrame)
          dists.push_back(make_pair(dist, make_pair(i, j)));
        else
          cout << "Not considering " << i << ":" << j << " which are " << dist << " apart" << endl;
      }
    }
  // Sort it by distance, so that closer matches come first.
  sort(dists.begin(), dists.end());

  int numPossibleMatches = min(numActiveTracks, (int) allEyes.size());
  int numMatches = 0;

  auto end = dists.end();
  for (auto it = dists.begin(); it != end && numMatches < numPossibleMatches;) {
    const pair<int, int> &edge = it->second;
    result.push_back(edge);
    ++numMatches;
    // Step over the current item ...
    ++it;
    // ... and discard any items that would conflict with it
    end = remove_if(it, end, Match(edge.first, edge.second));
  }

  return result;
}

void record(EyeTrack& track, const Mat& img, const Rect& eyeSrc) {
  // Make a new rectangle centred on the same point, and as wide,
  // but with the target aspect ratio
  const double eyeAspect = double(eyeSize.height) / eyeSize.width;
  Rect srcRect(0, 0, eyeSrc.width, eyeSrc.width * eyeAspect);
  srcRect -= center(srcRect);
  srcRect += center(eyeSrc);

  track.frames.push_back(Mat());
  resize(img(srcRect), track.frames.back(), eyeSize, 0, 0, INTER_LINEAR);
  track.lastSeen.push_back(eyeSrc);
  track.numFramesSinceLastSeen = 0;
}

void detect(Mat& img, CascadeClassifier& cascade,
    CascadeClassifier& nestedCascade, double scale) {
  int i = 0;
  double t = 0;
  vector<Rect> faces;
  Mat gray, smallImg;

  cvtColor(img, gray, CV_BGR2GRAY);
  resize(gray, smallImg, Size(), 1 / scale, 1 / scale, INTER_LINEAR);
  equalizeHist(smallImg, smallImg);

  t = (double) cvGetTickCount();
  cascade.detectMultiScale(smallImg, faces, 1.1, 2, 0
  //|CV_HAAR_FIND_BIGGEST_OBJECT
  //|CV_HAAR_DO_ROUGH_SEARCH
      | CV_HAAR_SCALE_IMAGE, Size(30, 30));
  t = (double) cvGetTickCount() - t;
//  printf("detection time = %g ms\n",
//      t / ((double) cvGetTickFrequency() * 1000.));

  // Bounding boxes of all eyes found this frame, in full-size camera space
  vector<Rect> allEyes;
  for (vector<Rect>::const_iterator r = faces.begin(); r != faces.end();
      r++, i++) {
    Mat smallImgROI;
    vector<Rect> nestedObjects;

    if (nestedCascade.empty())
      continue;
    smallImgROI = smallImg(*r);
    nestedCascade.detectMultiScale(smallImgROI, nestedObjects, 1.1, 2, 0
    //|CV_HAAR_FIND_BIGGEST_OBJECT
    //|CV_HAAR_DO_ROUGH_SEARCH
    //|CV_HAAR_DO_CANNY_PRUNING
        | CV_HAAR_SCALE_IMAGE, Size(30, 30));

    for (vector<Rect>::const_iterator e = nestedObjects.begin();
        e != nestedObjects.end(); ++e) {
      allEyes.push_back((*e + r->tl()) * scale);
    }
  }

  // Find which tracked eyes are closest to the observed eyes this frame
  vector<pair<int, int> > matching = assignTracksToEyes(eyeTracks, allEyes);
  vector<bool> foundTracks(eyeTracks.size());
  vector<bool> foundEyes(allEyes.size());
  for (vector<pair<int, int> >::const_iterator trackToEye = matching.begin();
      trackToEye != matching.end(); ++trackToEye) {
    foundTracks[trackToEye->first] = true;
    foundEyes[trackToEye->second] = true;
    const Rect& eyeSrc = allEyes[trackToEye->second];
    EyeTrack& track = eyeTracks[trackToEye->first];

    record(track, img, eyeSrc);
  }

  // Forget any tracks that haven't been picked up for a while
  for (int i = 0; i < eyeTracks.size(); ++i) {
    if (!eyeTracks[i].lastSeen.empty() && !foundTracks[i]) {
      int framesSinceLastSeen = ++(eyeTracks[i].numFramesSinceLastSeen);
      if (framesSinceLastSeen > 5)
        eyeTracks[i] = EyeTrack();
    }
  }

  // Put any new eyes into a free slot, if possible
  int j = 0;
  for (int i = 0; i < allEyes.size(); ++i) {
    if (!foundEyes[i]) {
      while (j < eyeTracks.size() && !eyeTracks[j].lastSeen.empty())
        j++;
      if (j == eyeTracks.size())
        break;
      record(eyeTracks[j], img, allEyes[i]);
    }
  }

  for (vector<EyeTrack>::iterator t = eyeTracks.begin(); t != eyeTracks.end();
      ++t) {
    if (t->frames.size() >= 10) {
      t->frames.erase(t->frames.begin(), t->frames.end() - 10);
      t->lastSeen.erase(t->lastSeen.begin(), t->lastSeen.end() - 10);
    }
  }

  i = 0;
  for (vector<Rect>::const_iterator r = faces.begin(); r != faces.end();
      ++r, ++i) {
    Scalar color = colors[i % 8];
    rectangle(img, *r * scale, color, 3);
  }

  i = 0;
  // for(vector<Rect>::const_iterator e = allEyes.begin(); e != allEyes.end(); ++e, ++i) {
  //   const Scalar& color = colors[i % maxEyes];
  //   rectangle(img, *e, color, 3);
  // }
}
