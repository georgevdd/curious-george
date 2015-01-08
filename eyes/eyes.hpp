#ifndef EYES_EYES_HPP
#define EYES_EYES_HPP

#include "opencv2/objdetect/objdetect.hpp"

void detect( cv::Mat& img,
             cv::CascadeClassifier& cascade, cv::CascadeClassifier& nestedCascade,
             double scale);
void showEyes();
void drawTracks(cv::Mat& img);

#endif // EYES_EYES_HPP
