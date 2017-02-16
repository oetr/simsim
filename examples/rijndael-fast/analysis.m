#############################################################
## Get data from the leakage file
#############################################################
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

#############################################################
## Simple CPA
#############################################################
source("sbox-and-hws.m") ## load sbox and hamming weights tables
guessed_key = zeros(1,16,"uint8");
corrs = zeros(16,nVals);
for key_byte = 1:16
  guess = repmat(0:255, nTraces,1);
  pt_repeated = repmat(plaintexts(:,key_byte), 1, 256);
  hypotheses = hw(sbox(bitxor(guess,pt_repeated)+1)+1);
  c = abs(corr(hypotheses,leakage));
  guessed_key(key_byte) = find(max(c') == max(max(c)))-1;
  corrs(key_byte,:) = c(guessed_key(key_byte)+1,:);
end

printf("%d key bytes correct\n", sum(guessed_key == keys(1,:)));

#############################################################
## Make nice plots
#############################################################
set(0, 'DefaulttextInterpreter', 'none')
figure(1)
papersize = [30,21]*0.4;
set (gcf, "paperorientation", "landscape") 
set (gcf, "papersize", papersize)
set (gcf, "paperposition", [0 0, papersize])
fontSans = 'DejaVuSans'
fontSerif = 'DejaVuSerif'
fontMono = "Inconsolata"

plot(corrs'+repmat(0:15,nVals,1))
xlabel("clock cycle")
ylabel("correlation for each winning key byte")
set (gca, 'ytick', 0:16)
ylim([0 16])
xlim([0 nVals])
title("CPA on each key byte")
print("CPA-each-keybyte.png")

plot(cc(1,:),mean(leakage))
ylabel("leakage")
xlabel("clock cycle")
xlim([0 cc(end)])
title("Mean leakage of 1000 traces")
print("leakage.png")

#############################################################
## Get the data from the execution trace
#############################################################
traceFile = "execution-trace.txt";
[instrCC,instrAddr,instrOPcode,instrName,instrDuration,instrGenericName,instrIDs,instrAnnotation] = textread(traceFile, '%d|%d|%d|%s|%d|%s|%d|%s' ,'delimiter' , '|', 'whitespace', "\n");
instrName = char(instrName);
instrCC = double(instrCC);
instrDuration = double(instrDuration);
instrIDs = double(instrIDs);
instrGenericName = char(instrGenericName);
instrAnnotation = char(instrAnnotation);

## Each instruction has a unique ID in the simulator
## After plotting them, I can recognize the key scheduler
## and 9 rounds
plot(instrCC,instrIDs)
ylabel("instruction ID")
xlabel("clock cycle")
xlim([0 instrCC(end)])
print("instr-id.png")

#############################################################
## Let's combine the information from both files to figure out
## which instructions are responsible for the success of CPA
#############################################################
## find out the region where CPA is successful (first round)
#region = find(sum(corrs>(mean(corrs') + std(corrs')*4)'));
region = find(corrs(1,:)>(mean(corrs(1,:)) + std(corrs(1,:))*4));
region = region(1)-5:region(end)+5;
maxCorrCC = region(find(corrs(1,region)==max(corrs(1,region))))-1;
ccsIDs = find((instrCC >= region(1)) & (instrCC <= region(end)));
startCC = instrCC(ccsIDs(1));
endCC = instrCC(ccsIDs(end));
base = 1;
pos = 0;
instrModulo = round((endCC-startCC+1)/5);
incr = 0.1;
ylow = 0;
baseInstr = 0;
yhigh = base+baseInstr+(instrModulo+1)*incr;

plot(region,corrs(1,region))
hold on

for i = ccsIDs'
  startInstr = instrCC(i);
  endInstr = instrCC(i)+instrDuration(i);
  samples = [startInstr endInstr];
  ## Vertical lines, separating instructions
  plot([samples(1) samples(1)], [yhigh ylow],"--","linewidth",2, "color", [0.7,0.7,0.7]);
  ## draw instruction name
  textColor = [0,0,0];
  if ((instrCC(i) <= maxCorrCC) & (instrCC(i+1) > maxCorrCC))
    textColor = [0,0,1];
  end
  text(samples(1), ...
       base+baseInstr+(pos+1)*incr, ...
       instrAnnotation(i,:),'fontsize',10, ...
       "fontname",fontMono,"color",textColor);
  ## draw instruction duration (clock cycles)
  durationInstr = num2str(instrDuration(i));
  text((samples(1) + samples(2))/2, base+baseInstr, ...
       durationInstr,'fontsize',12,"fontname",fontMono);
  pos = mod(pos + 1, instrModulo);
endfor;
set (gca, 'ytick', 0:0.1:1)
title("Instruction causing most HW leakage")
xlabel("clock cycle")
ylabel("correlations")
ylim([0,yhigh])
hold off;
print("Instruction-causing-leakage.png")

