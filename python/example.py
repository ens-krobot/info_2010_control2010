# example.py
# ----------
# Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
# Licence   : BSD3
#
# This file is a part of [kro]bot.

import krobot, time

# +-------------------------------------------------------------------+
# | Move the claws                                                    |
# +-------------------------------------------------------------------+

claws = krobot.Claws()
claws.enable()

claws.open()
time.sleep(0.5)
claws.close()
time.sleep(0.5)

claws.open()
time.sleep(0.5)
claws.close()
time.sleep(0.5)

claws.disable()

# +-------------------------------------------------------------------+
# | Move the robot                                                    |
# +-------------------------------------------------------------------+

motors = krobot.Motors()

motors.move(distance=0.1, velocity=0.4, acceleration=0.8)
motors.move(distance=-0.1, velocity=0.4, acceleration=0.8)

motors.move(distance=0.1, velocity=0.4, acceleration=0.8)
motors.move(distance=-0.1, velocity=0.4, acceleration=0.8)
