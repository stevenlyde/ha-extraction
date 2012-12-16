while true
  Tc = readTemp();
  physical(Tc);
  if Tc < 30
    heat(true);
    assert_cps('(= dTc 4)');
  else
    heat(false);
    assert_cps('(= dTc -1)');
  end
end

