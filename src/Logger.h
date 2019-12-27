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

#define VEX_TRACE(...)    ::Logger::get_logger()->trace(__VA_ARGS__)
#define VEX_INFO(...)     ::Logger::get_logger()->info(__VA_ARGS__)
#define VEX_WARN(...)     ::Logger::get_logger()->warn(__VA_ARGS__)
#define VEX_ERROR(...)    ::Logger::get_logger()->error(__VA_ARGS__)
#define VEX_CRITICAL(...) ::Logger::get_logger()->critical(__VA_ARGS__)
#define VEX_ASSERT(x, ...) { if(!(x)) { VEX_ERROR(__VA_ARGS__); exit(EXIT_FAILURE); } }
#define VEX_ASSERT_PTR(x, ...) { if(!(x)) { VEX_ERROR(__VA_ARGS__); return nullptr; } }