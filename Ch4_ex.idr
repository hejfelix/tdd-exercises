module Ch4_ex

import Data.Vect

data PowerSource = Petrol | Pedal | Electric

data Vehicle : PowerSource -> Type
  where
    Bicycle   : Vehicle Pedal
    Unicycle  : Vehicle Pedal
    Tesla     : (power: Nat) -> Vehicle Electric
    Car       : (fuel : Nat) -> Vehicle Petrol
    MotorCycle: (fuel : Nat) -> Vehicle Petrol
    Bus       : (fuel : Nat) -> Vehicle Petrol

total wheels : Vehicle power -> Nat
wheels Bicycle        = 2
wheels Unicycle       = 1
wheels (Tesla _)      = 4
wheels (Car _)        = 4
wheels (MotorCycle _) = 2
wheels (Bus _)        = 4

total refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Car _)          = Car 100
refuel (MotorCycle _)   = MotorCycle 30
refuel (Bus _)          = Bus 200

total recharge : Vehicle Electric -> Vehicle Electric
recharge (Tesla _) = Tesla 90


vectTake : (n : Nat) -> Vect (n + m) elem -> Vect n elem
vectTake Z xs = []
vectTake (S k) (x :: xs) = x :: vectTake k xs


sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} pos xs ys =
  case integerToFin pos n of
    Nothing   => Nothing
    Just idx  => Just (index idx xs + index idx ys)
