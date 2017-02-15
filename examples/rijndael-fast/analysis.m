#############################################
## Get data from the leakage file
#############################################
f       = fopen("leakage-trace.txt");
nTraces = fread(f, 1, "*uint32","b"); ## "b" - big endian
keys = zeros(nTraces,16,"uint8");
plaintexts = zeros(nTraces,16,"uint8");
ciphertexts = zeros(nTraces,16,"uint8");
leakage = cell(nTraces,1);
pc = cell(nTraces,1);
cc = cell(nTraces,1);
for i = 1:nTraces
  keys(i,:) = fread(f,16,"*uint8");
  plaintexts(i,:) = fread(f,16,"*uint8");
  ciphertexts(i,:) = fread(f,16,"*uint8");
  nVals   = fread(f, 1, "*uint32","b"); ## "b" - big endian
  leakage{i} = fread(f,nVals,"*double", "n")'; ## "n" - native
  pc{i}      = fread(f,nVals,"*uint16", "b")';
  cc{i}      = fread(f,nVals,"*uint16", "b")';
end
fclose(f);
## Conversion from cell to matrix only works if there are no timing
## differences between the execution runs
leakage = cell2mat(leakage);
pc = cell2mat(pc);
cc = cell2mat(cc);

## Add extra noise, because some instructions have constant leakage,
## which will result in zero variance
leakage = leakage + rand(size(leakage))+0.0001;

#############################################
## Simple CPA
#############################################
## load sbox and hamming weights tables
source("sbox-and-hws.m")
## allocate
guessed_key = zeros(1,16,"uint8");
corrs = zeros(16,nVals);
## perform CPA for each key byte
for key_byte = 1:16
  ## make a guess for a key byte, repeat for each trace
  guess = repmat(0:255, nTraces,1);
  pt_repeated = repmat(plaintexts(:,key_byte), 1, 256);
  hypotheses = hw(sbox(bitxor(guess,pt_repeated)+1)+1);
  c = abs(corr(hypotheses,leakage));
  guessed_key(key_byte) = find(max(c') == max(max(c)))-1;
  corrs(key_byte,:) = c(guessed_key(key_byte)+1,:);
end

printf("%d key bytes correct\n", sum(guessed_key == keys(1,:)));


## Make nice plots
set(0, 'DefaulttextInterpreter', 'none')
figure(1)
papersize = [30,21]*0.5;
set (gcf, "paperorientation", "landscape") 
set (gcf, "papersize", papersize)
set (gcf, "paperposition", [0 0, papersize])

plot(corrs'+repmat(0:15,nVals,1))
xlabel("clock cycle")
ylabel("correlation for each winning key byte")
set (gca, 'ytick', 0:16)
ylim([0 16])
xlim([0 nVals])
title("CPA on each key byte")
print("CPA-each-keybyte.png")

plot(cc(1,:),leakage(1,:))
## plot(cc(1,:),mean(leakage))
ylabel("mean leakage")
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
