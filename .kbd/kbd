#!/usr/bin/env ruby

BIN = "xkbcomp -w 0 -R/usr/share/X11/xkb/"
KEYBOARD_DIR = "~/.kbd/"
ARG = "${DISPLAY}"

LAYOUTS = %w[ vaio_bepo tkfc_bepo ]

def pick_keyboard_layout  
  puts "layouts?"
  LAYOUTS.each_with_index { |e, i| puts "#{i} ->  #{e}" }  
  LAYOUTS[gets.to_i]
end
  

def run_keyboard_conf(layout)
  cmd = "#{BIN} #{KEYBOARD_DIR}#{layout} #{ARG}"
  p cmd
  `#{cmd}`
end

run_keyboard_conf(pick_keyboard_layout)
