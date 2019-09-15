prologues := 1; % 3 = include font to PostScript

input TEX;
input tdecimal;

% parameters
u = 1cm;
dpi = 96;


%  ( a )b----------weight--c(   d   )
%   r = radius of each circle
%   ra radius of circle a (distance from a to b)
%   rb radius of circle d (distance from d to c)
def weightline (expr xa, ya, ra, xd, yd, rd, shift) (text weight) =
  begingroup
    % /10 = fix overflow
    R := sqrt(((xd/10 - xa/10)**2) + ((yd/10 - ya/10)**2)) * 10;

    xb := ((xd - xa) * ra / R) + xa;
    yb := ((yd - ya) * ra / R) + ya;

    xc := ((xa - xd) * rd / R) + xd;
    yc := ((ya - yd) * rd / R) + yd;

    draw (xb,yb)--(xc,yc);

    label.top (weight, origin) rotated angle((xc,yc)-(xb,yb)) shifted shift[(xb,yb),(xc,yc)];
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
  n_sensor = 6;
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
  n_association = 5;
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
      weightline (x_sensor, j*y_sensor, r_sensor, x_association, i*y_association, r_association, 0.9)(tdecimal(w, 3));
    endfor
    label(TEX("$\sum = " & tdecimal(sum, 2) & "$"), (x_association, i*y_association));
    draw a shifted (x_association, i*y_association);
  endfor
  %
  % fixme: static numbers (need calculate?)
  numeric associations[];
  associations1 = 1;
  associations2 = 0;
  associations3 = 1;
  associations4 = 1;
  associations5 = 0;
  label(TEX("$>1 \Rightarrow" & decimal associations1 & "$"), (x_association, 0*y_association - 10));
  label(TEX("$<1000 \Rightarrow" & decimal associations2 & "$"), (x_association, 1*y_association - 10));
  label(TEX("$>1 \Rightarrow" & decimal associations3 & "$"), (x_association, 2*y_association - 10));
  label(TEX("$>1 \Rightarrow" & decimal associations4 & "$"), (x_association, 3*y_association - 10));
  label(TEX("$<9000 \Rightarrow" & decimal associations5 & "$"), (x_association, 4*y_association - 10));



  x_response = 400;
  n_response = 4;
  y_response = ((n_sensor - 1) * y_sensor / (n_response - 1));
  r_response = 40;
  label("Response", (x_response, -100));
  path a;
  a = fullcircle scaled (r_response * 2);
  for i = 0 upto (n_response - 1):
    sum := 0;
    for j = 0 upto (n_association - 1):
      w := (i+1)*10 + i+1 + (j+1)/10.0 + (j+1)/100.0;
      sum := sum + w*associations[j+1];
      weightline (x_association, j*y_association, r_association, x_response, i*y_response, r_response, 0.85)(tdecimal(w, 3));
    endfor
    label(TEX("$\sum = " & tdecimal(sum, 2) & "$"), (x_response, i*y_response));
    draw a shifted (x_response, i*y_response);
  endfor
  numeric responses[];
  responses1 = 0;
  responses2 = 0;
  responses3 = 1;
  responses4 = 1;
  label(TEX("$<100 \Rightarrow" & decimal responses1 & "$"), (x_response, 0*y_response - 10));
  label(TEX("$<100 \Rightarrow" & decimal responses2 & "$"), (x_response, 1*y_response - 10));
  label(TEX("$>1 \Rightarrow" & decimal responses3 & "$"), (x_response, 2*y_response - 10));
  label(TEX("$>1 \Rightarrow" & decimal responses4 & "$"), (x_response, 3*y_response - 10));



  x_machine_learning = 500;
  n_machine_learning = 4;
  y_machine_learning = ((n_sensor - 1) * y_sensor / (n_response - 1));
  r_machine_learning = 40;
  label("correct answer", (x_machine_learning, -100));
  path a;
  a = fullcircle scaled (r_machine_learning * 2);
  numeric machine_learnings[];
  machine_learnings1 = 0;
  machine_learnings2 = 1;
  machine_learnings3 = 0;
  machine_learnings4 = 1;
  for i = 0 upto (n_machine_learning - 1):
    draw a shifted (x_machine_learning, i*y_machine_learning);
    label(decimal machine_learnings[i+1], (x_machine_learning, i*y_machine_learning));
  endfor
  label(btex $=$ etex, ((x_response + x_machine_learning)/2, 0*y_response)) withcolor green;
  label(btex $\ne$ etex, ((x_response + x_machine_learning)/2, 1*y_response)) withcolor red;
  weightline (x_association, 0*y_association, r_association, x_response, 1*y_response, r_response, 1.04)("+1");
  weightline (x_association, 2*y_association, r_association, x_response, 1*y_response, r_response, 1.05)("+1");
  weightline (x_association, 3*y_association, r_association, x_response, 1*y_response, r_response, 1.03)("+1");
  label(btex $\ne$ etex, ((x_response + x_machine_learning)/2, 2*y_response)) withcolor red;
  weightline (x_association, 0*y_association, r_association, x_response, 2*y_response, r_response, 1.03)("-1");
  weightline (x_association, 2*y_association, r_association, x_response, 2*y_response, r_response, 1.06)("-1");
  weightline (x_association, 3*y_association, r_association, x_response, 2*y_response, r_response, 1.06)("-1");
  label(btex $=$ etex, ((x_response + x_machine_learning)/2, 3*y_response)) withcolor green;

endfig;


end.
