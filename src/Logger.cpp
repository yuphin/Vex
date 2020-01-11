#include "Logger.h"

namespace Vex {
	std::shared_ptr<spdlog::logger> Logger::s_logger;
	void  Logger::init() {
		spdlog::set_pattern("%n: %v%$");
		s_logger = spdlog::stdout_color_mt("Vex");
		s_logger->set_level(spdlog::level::trace);
	}


	void Logger::set_printer_mode() {
		s_logger->set_pattern("%v%$");
	}


	void Logger::set_default_mode() {
		s_logger->set_pattern("%n: %v%$");

	}
}

