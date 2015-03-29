#ifndef EYES_EYES_HPP
#define EYES_EYES_HPP

#include <vector>

#include "opencv2/objdetect/objdetect.hpp"

struct EyeTrack {
  std::vector<cv::Rect> lastSeen;
  std::vector<cv::Mat> frames;
  int numFramesSinceLastSeen;

  EyeTrack(): numFramesSinceLastSeen(0) {}
};

void detect( cv::Mat& img,
             cv::CascadeClassifier& cascade, cv::CascadeClassifier& nestedCascade,
             double scale);
void showEyes();
void drawTracks(cv::Mat& img);

#endif // EYES_EYES_HPP
