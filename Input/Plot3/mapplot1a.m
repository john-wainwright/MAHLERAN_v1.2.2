function y = mapplot1a (run_no)

if run_no < 10
    qfile=strcat('dschg00', num2str(run_no,'%1i'),'.asc');
    dfile=strcat('depth00', num2str(run_no,'%1i'),'.asc');
    vfile=strcat('veloc00', num2str(run_no,'%1i'),'.asc');
    thfile=strcat('theta00', num2str(run_no,'%1i'),'.asc');
    sedfile=strcat('sedtr00', num2str(run_no,'%1i'),'.asc');
    nutfile=strcat('nutri00', num2str(run_no,'%1i'),'.asc');
elseif run_no < 100
    qfile=strcat('dschg0', num2str(run_no,'%2i'),'.asc');
    dfile=strcat('depth0', num2str(run_no,'%2i'),'.asc');
    vfile=strcat('veloc0', num2str(run_no,'%2i'),'.asc');
    thfile=strcat('theta0', num2str(run_no,'%2i'),'.asc');
    sedfile=strcat('sedtr0', num2str(run_no,'%2i'),'.asc');
    nutfile=strcat('nutri0', num2str(run_no,'%2i'),'.asc');
elseif run_no < 1000
    qfile=strcat('dschg', num2str(run_no,'%3i'),'.asc');
    dfile=strcat('depth', num2str(run_no,'%3i'),'.asc');
    vfile=strcat('veloc', num2str(run_no,'%3i'),'.asc');
    thfile=strcat('theta', num2str(run_no,'%3i'),'.asc');
    sedfile=strcat('sedtr', num2str(run_no,'%3i'),'.asc');
    nutfile=strcat('nutri', num2str(run_no,'%3i')),'.asc';
end

[X Y Z] = ascread1 (qfile);

subplot(2,3,1);imagesc(X, Y, Z);axis ('equal', 'tight')
colorbar;
title('total discharge  l');

[X Y Z] = ascread1 (dfile);

subplot(2,3,2);imagesc(X, Y, Z);axis ('equal', 'tight')
colorbar;
title('maximum depth  mm');

[X Y Z] = ascread1 (vfile);

subplot(2,3,3);imagesc(X, Y, Z);axis ('equal', 'tight')
colorbar;
title('maximum velocity  mm s^-^1');

[X Y Z] = ascread1 (thfile);

subplot(2,3,4);imagesc(X, Y, Z);axis ('equal', 'tight')
colorbar;
title('final surface soil moisture  m^3 m^-^3');

%[X Y Z] = ascread1 (sedfile);

%subplot(2,3,5);imagesc(X, Y, Z, [0 2*median(median(Z))]);axis ('equal', 'tight')
subplot(2,3,5);imagesc(X, Y, Z);axis ('equal', 'tight')
colorbar;
title('sediment flux  kg');

[X Y Z] = ascread1 (nutfile);

subplot(2,3,6);imagesc(X, Y, Z);axis ('equal', 'tight')
colorbar;
title('ammonium flux  mg');
