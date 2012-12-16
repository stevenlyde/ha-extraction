
while true
  t = readTemp()
  physical(t)
  assert_cps('(= dt 34)')  % rate of increase with no rods
  if (t > 1100)  % maximal tolerable temperature

    x1 = timeRod1Extracted()
    assert_cps('(= dx1 1)')
    physical(x1)
    x2 = timeRod2Extracted()
    assert_cps('(= dx2 1)')
    physical(x2)

    if (x1 >= 80)  % minimal delay between reuse of a rod
      insertRod1()
      assert_cps('(= dt 25)')  % rate of increase with rod 1 present
      while true
        t = readTemp()
        physical(t)
        if (t <= 250)  % minimal tolerable temperature
          break
        end
      end
      removeRod1()
      set_physical('(= x1 0)')
    elseif (x2 >= 80) % minimal delay between reuse of a rod
      insertRod2()
      assert_cps('(= dt 10)')  % rate of increase with rod 2 present
      while true
        t = readTemp()
        physical(t)
        if (t <= 250)  % minimal tolerable temperature
          break
        end
      end
      removeRod2()
      set_physical('(= x2 0)')
    else
      shutdown()
      assert_cps('(= dt 1)')
      break
    end
  end
end

