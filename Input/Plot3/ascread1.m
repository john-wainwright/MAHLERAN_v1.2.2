function [X Y Z] = ascread1 (filename);

cols = 0;
rows = 0;
x_min = 0.;
y_min = 0.;
cellsize = 0;
nodata = -9999;
fid = fopen (filename);
a1 = textscan (fid,'%s %d',1);
cols = a1{2};
a2 = textscan (fid,'%s %d',1);
rows = a2{2};
a3 = textscan (fid,'%s %f',1);
x_min = a3{2};
a4 = textscan (fid,'%s %f',1);
y_min = a4{2};
a5 = textscan (fid,'%s %f',1);
cellsize = a5{2};
a6 = textscan (fid,'%s %f',1);
nodata = a6{2};

x_max = x_min + cellsize * cols;
y_max = y_min + cellsize * rows;

Z = zeros(rows - 2, cols - 2);
%DATA = zeros (rows * cols);

DATA = textscan(fid,'%f',-1);

X = zeros ((rows - 2) * (cols - 2), 1);
Y = zeros ((rows - 2) * (cols - 2), 1);

k = 1;
for i = 2:rows - 1
    for j = 2:cols - 1
        Z (i - 1, j - 1) = DATA{1}((i - 1) * cols + j);
        X (k) = x_min + cellsize * double (j);
        Y (k) = y_min + cellsize * double (i);
        k = k + 1;
    end
end
