cmake_minimum_required(VERSION 2.8)
project( eyes )
find_package( OpenCV REQUIRED )
add_executable( eyes eyes.cpp main.cpp )
target_link_libraries( eyes ${OpenCV_LIBS} )

add_executable( eyes_test eyes_test.cpp )
