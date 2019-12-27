#pragma once
#include "spdlog/spdlog.h"
#include "spdlog/fmt/ostr.h"
#include "spdlog/sinks/stdout_color_sinks.h"
#include <memory>
namespace Vex {
	class Logger {
		public:
		static void init();
		inline static std::shared_ptr<spdlog::logger>& get_logger() { return s_logger; }
		private:
		static std::shared_ptr<spdlog::logger> s_logger;
	};

}

#define VEX_TRACE(...)    ::Vex::Logger::get_logger()->trace(__VA_ARGS__)
#define VEX_INFO(...)     ::Vex::Logger::get_logger()->info(__VA_ARGS__)
#define VEX_WARN(...)     ::Vex::Logger::get_logger()->warn(__VA_ARGS__)
#define VEX_ERROR(...)    ::Vex::Logger::get_logger()->error(__VA_ARGS__)
#define VEX_CRITICAL(...) ::Vex::Logger::get_logger()->critical(__VA_ARGS__)
