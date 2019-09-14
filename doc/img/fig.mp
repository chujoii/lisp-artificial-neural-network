prologues := 1; % 3 = include font to PostScript

% parameters
u = 1cm;
dpi = 96;

beginfig(1);
  save w, h, xmin, xmax, ymin, ymax;
  
  w = 1024;
  h = 768;
  
  xmax = w/dpi;
  ymax = h/dpi;
  xmin = ymin = 0.0;

  x_sensor = 0;
  n_sensor = 5;
  y_sensor = 100;
  label("Sensor", (x_sensor, -100));
  path a;
  a = fullcircle scaled (1*u);
  for i = 0 upto (n_sensor - 1):
    label(decimal ((i+1)*y_sensor), (x_sensor, i*y_sensor));
    draw a shifted (x_sensor, i*y_sensor);
  endfor


  x_association = 200;
  n_association = 4;
  y_association = ((n_sensor - 1) * y_sensor / (n_association - 1));
  label("Association", (x_association, -100));
  path a;
  a = fullcircle scaled (3*u);
  for i = 0 upto (n_association - 1):
    label("sum=" & decimal ((i+1)*y_association), (x_association, i*y_association));
    draw a shifted (x_association, i*y_association);
  endfor


  x_response = 400;
  n_response = 3;
  y_response = ((n_sensor - 1) * y_sensor / (n_response - 1));
  label("Response", (x_response, -100));
  path a;
  a = fullcircle scaled (2*u);
  for i = 0 upto (n_response - 1):
    label("sum=" & decimal ((i+1)*y_response), (x_response, i*y_response));
    draw a shifted (x_response, i*y_response);
  endfor
  
endfig;


end.
