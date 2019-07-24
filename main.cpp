#include <iostream>

#include "wektor.hpp"

int main([[maybe_unused]] int argc, [[maybe_unused]] char** argv) {
  hqrp::wektor foo = { 2, 1, 3, 7 };
  for (auto x : foo) {
    std::cout << x << std::endl;
  }
}