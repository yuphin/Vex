#pragma once
#include "spdlog/spdlog.h"
#include "spdlog/fmt/ostr.h"
#include "spdlog/sinks/stdout_color_sinks.h"
#include <memory>
namespace Vex {
	class Logger {
		public:
		static void init();
		static void set_printer_mode();
		static void set_default_mode();
		inline static std::shared_ptr<spdlog::logger>& get_logger() { return s_logger; }
		private:
		static std::shared_ptr<spdlog::logger> s_logger;
	};

}

#define VEX_TRACE(...)    ::Vex::Logger::get_logger()->trace(__VA_ARGS__)
#ifdef DEBUG
	#define VEX_INFO(...) ::Vex::Logger::get_logger()->info(__VA_ARGS__)
#else
	#define VEX_INFO(...)	
#endif
#define VEX_WARN(...)     ::Vex::Logger::get_logger()->warn(__VA_ARGS__)
#define VEX_ERROR(...)    ::Vex::Logger::get_logger()->error(__VA_ARGS__)
#define VEX_CRITICAL(...) ::Vex::Logger::get_logger()->critical(__VA_ARGS__)
#define VEX_ASSERT(x, ...) { if(!(x)) { VEX_ERROR(__VA_ARGS__); exit(EXIT_FAILURE); } }
#define VEX_ASSERT_PTR(x, ...) { if(!(x)) { VEX_ERROR(__VA_ARGS__); return nullptr; } }
