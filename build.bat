@echo off

del game.nes
Z:\NESDevKit\cc65\bin\ca65 --verbose --target nes Main.s
Z:\NESDevKit\cc65\bin\ld65 --target nes Main.o -o Game.nes 
