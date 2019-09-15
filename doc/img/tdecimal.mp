vardef tdecimal(expr num, d) :=
  % author: Marcel Krüger
  % date: 2019-Jan-19 at 14:28
  % url: https://tex.stackexchange.com/questions/470856/how-to-limit-amount-of-numbers-after-comma-in-metapost
  % title: How to limit amount of numbers after comma in metapost
  save numS; string numS; numS = decimal(num);
  save j; j := length numS;
  for k = 0 upto length numS - 1:
    if "." = substring (k,k+1) of numS: j:=k; fi
  endfor
  for k = j + 1 upto j + d:
    if "0" <> substring (k,k+1) of numS: j:=k+1; fi
  endfor
  substring (0,j) of numS
enddef;
