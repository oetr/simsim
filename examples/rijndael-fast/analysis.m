## Make nice plots
set(0, 'DefaulttextInterpreter', 'none')
figure(1)
papersize = [30,21]*0.3;
set (gcf, "paperorientation", "landscape") 
set (gcf, "papersize", papersize)
set (gcf, "paperposition", [0 0, papersize])

## Get data from the leakage file
f       = fopen("leakage-trace.txt");
nVals   = fread(f, 1, "*uint32","b"); ## "b" - big endian
leakage = fread(f,nVals,"*double", "n"); ## "n" - native
pc      = fread(f,nVals,"*uint16", "b");
cc      = fread(f,nVals,"*uint16", "b");
fclose(f);

plot(cc,leakage)
ylabel("leakage")
xlabel("clock cycle")
xlim([0 cc(end)])
print("leakage.png")

## Get the data from the execution trace
traceFile = "execution-trace.txt";
[instrCC,instrAddr,instrOPcode,instrName,instrDuration,instrGenericName,instrIDs,instrAnnotation] = textread(traceFile, '%d|%d|%d|%s|%d|%s|%d|%s' ,'delimiter' , '|', 'whitespace', "\n");
instrName = char(instrName);
instrCC = double(instrCC);
instrDuration = double(instrDuration);
instrIDs = double(instrIDs);
instrGenericName = char(instrGenericName);
instrAnnotation = char(instrAnnotation);

plot(instrCC,instrIDs)
ylabel("instruction ID")
xlabel("clock cycle")
xlim([0 instrCC(end)])
print("instr-id.png")
