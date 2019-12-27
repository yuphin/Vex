#include "Logger.h"

void Logger::info() {
	spdlog::info("Hello, {}!", "World");
}

void Logger::error() {}
