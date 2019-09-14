prologues := 1; % 3 = include font to PostScript

input TEX;

% parameters
u = 1cm;
dpi = 96;


%  ( a )b----------weight--c(   d   )
%   r = radius of each circle
%   ra radius of circle a (distance from a to b)
%   rb radius of circle d (distance from d to c)
def weightline (expr xa, ya, ra, xd, yd, rd, weight) =
  begingroup
    % /10 = fix overflow
    R := sqrt(((xd/10 - xa/10)**2) + ((yd/10 - ya/10)**2)) * 10;

    xb := ((xd - xa) * ra / R) + xa;
    yb := ((yd - ya) * ra / R) + ya;

    xc := ((xa - xd) * rd / R) + xd;
    yc := ((ya - yd) * rd / R) + yd;

    draw (xb,yb)--(xc,yc);

    label.top (decimal weight, origin) rotated angle((xc,yc)-(xb,yb)) shifted 0.9[(xb,yb),(xc,yc)];
  endgroup;
enddef;


beginfig(1);
  save w, h, xmin, xmax, ymin, ymax;
  
  w = 1024;
  h = 768;
  
  xmax = w/dpi;
  ymax = h/dpi;
  xmin = ymin = 0.0;

  x_sensor = 0.0;
  n_sensor = 5;
  y_sensor = 100.0;
  r_sensor = 15.0;
  step_sensor = 10;
  label("Sensor", (x_sensor, -100));
  path a;
  a = fullcircle scaled (r_sensor * 2);
  for i = 0 upto (n_sensor - 1):
    label(decimal ((i+1)*step_sensor), (x_sensor, i*y_sensor));
    draw a shifted (x_sensor, i*y_sensor);
  endfor


  x_association = 200.0;
  n_association = 4;
  y_association = ((n_sensor - 1) * y_sensor / (n_association - 1));
  r_association = 30.0;
  label("Association", (x_association, -100));
  path a;
  a = fullcircle scaled (r_association * 2);
  for i = 0 upto (n_association - 1):
    sum := 0;
    for j = 0 upto (n_sensor - 1):
      w := i+1 + (j+1)/10;
      sum := sum + w*(j+1)*step_sensor;
      weightline (x_sensor, j*y_sensor, r_sensor, x_association, i*y_association, r_association, w);
    endfor
    label(TEX("$\sum = " & decimal round(sum) & "$"), (x_association, i*y_association));
    draw a shifted (x_association, i*y_association);
  endfor



  x_response = 400;
  n_response = 3;
  y_response = ((n_sensor - 1) * y_sensor / (n_response - 1));
  r_response = 20;
  label("Response", (x_response, -100));
  path a;
  a = fullcircle scaled (r_response * 2);
  for i = 0 upto (n_response - 1):
    label("sum=" & decimal ((i+1)*y_response), (x_response, i*y_response));
    draw a shifted (x_response, i*y_response);
  endfor
  
endfig;


end.
