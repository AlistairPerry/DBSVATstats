function VATsummarydata(STNfile)
%extractallVATdata Extract VAT voxels from each subject
%   Detailed explanation goes here

%required software dependencies:
%NBS
%NIFTI

[~,STNdata]=read([STNfile]);

%setup working and subjects directories
workingdirectory = pwd;
files = dir(workingdirectory);
dirFlags=[files.isdir];
subFolders=files(dirFlags);
subFolders(1:2)=[];

%extract eg size of VAT images
demoRVATfile=[workingdirectory '/' 'Lead_DBS_002' '/' 'rLEAD_DBS_VAT_RIGHT.nii'];
[RVAThdr,RVATdata]=read(demoRVATfile);
numvoxels=numel(RVATdata);

%setup identity matrix for extracting VAT voxel information

VATmatdims=size(RVATdata);
VAT2Dmatsize=VATmatdims(1,1)*VATmatdims(1,2);
identmat=zeros(394,466,378);

for k=1:VATmatdims(1,2)
    for j=1:VATmatdims(1,1)
        for l=2:VATmatdims(1,3)
            identmat(j,k,l)=[identmat(j,k,l-1)]+VAT2Dmatsize;
        end
    end
end

%create output VAT structure for all subjects
reshapeRVATall=zeros(length(subFolders),numvoxels);

%load all subjects VATS and extract - start with R hemisphere for now
for s = 1:length(subFolders)
    currentSubj= subFolders(s,1).name;
    currentSubjDir = char([workingdirectory '/' currentSubj]);
    
    %parse VAT file string
    VATsubjids{s,1} = currentSubj;
    SubjRVATfile=[currentSubjDir '/' 'rLEAD_DBS_VAT_RIGHT.nii'];
    SubjLVATfile=[currentSubjDir '/' 'rLEAD_DBS_VAT_LEFT.nii'];
    
    %load subject VAT
    [RVAThdr,RVATdata]=read(SubjRVATfile);
    %VATdataall(:,:,:,i)=VATdata;
    
    reshapeRVATsubj=reshape(RVATdata,[1 numvoxels]);
    reshapeRVATall(s,:)=reshapeRVATsubj;
    
    [LVAThdr,LVATdata]=read(SubjLVATfile);
    %VATdataall(:,:,:,i)=VATdata;
    
    reshapeLVATsubj=reshape(LVATdata,[1 numvoxels]);
    reshapeLVATall(s,:)=reshapeLVATsubj;
end

fid = fopen(['VATsubijds.txt'], 'wt');
for s = 1:length(subFolders)
    fprintf(fid, '%s\n', VATsubjids{s,1});
end
fclose(fid)

%first right-hemisphere
%remove voxels with no VAT across all subjs
reshapeRVATallnonzero=reshapeRVATall;
VATcolmax=max(reshapeRVATallnonzero);
maxzero=find(VATcolmax==0);
[~,maxzero]=find(VATcolmax==0);
reshapeRVATallnonzero(:,maxzero)=[];

%write VAT data out as textfile

dlmwrite('RVATreshapeall.txt',reshapeRVATallnonzero,'delimiter','\t');

% + voxels extracted
Ridentmatkeep=1:numvoxels;
Ridentmatkeep(:,maxzero)=[];

dlmwrite('RVATdatavoxelinfo.txt',Ridentmatkeep,'delimiter','\t');

%now as nii file for visualisation purposes
RVATdataallvoxels=zeros(size(RVATdata));
RVATdataallvoxels(Ridentmatkeep)=1;
write(RVAThdr,RVATdataallvoxels,'RVATdataallvoxels.nii')

%determine most consistent voxels - potentially for mask in fsl randomise masking
thrperc=0.25;
thrreq=floor(thrperc*length(subFolders));

sumreshapeRVATall=sum(reshapeRVATallnonzero,1);
[~,RVATabvthrcol]=find(sumreshapeRVATall>=thrreq);

Ridentmatkeepabvthr=Ridentmatkeep(RVATabvthrcol);

RVATdataabvthr=zeros(size(RVATdata));
RVATdataabvthr(Ridentmatkeepabvthr)=1;
write(RVAThdr,RVATdataabvthr,'RVATdataabvthr.nii')

%density - across subjects
densreshapeRVAT=sumreshapeRVATall./(length(subFolders));
RVATdatadensity=zeros(size(RVATdata));

for i = 1:length(Ridentmatkeep)
RVATdatadensity(Ridentmatkeep(1,i))=densreshapeRVAT(1,i);  
end

nii = make_nii(RVATdatadensity, [0.5 0.5 0.5], [0 0 0], 64);
save_nii(nii, 'RVATdatadensity.nii');

%now left-hemisphere
%remove voxels with no VAT across all subjs
reshapeLVATallnonzero=reshapeLVATall;
VATcolmax=max(reshapeLVATallnonzero);
maxzero=find(VATcolmax==0);
[~,maxzero]=find(VATcolmax==0);
reshapeLVATallnonzero(:,maxzero)=[];

%write VAT data out as textfile

dlmwrite('LVATreshapeall.txt',reshapeLVATallnonzero,'delimiter','\t');

% + information of voxels extracted
Lidentmatkeep=1:numvoxels;
Lidentmatkeep(:,maxzero)=[];

dlmwrite('LVATdatavoxelinfo.txt',Lidentmatkeep,'delimiter','\t');

%now as nii file for visualisation purposes
LVATdataallvoxels=zeros(size(LVATdata));
LVATdataallvoxels(Lidentmatkeep)=1;
write(LVAThdr,LVATdataallvoxels,'LVATdataallvoxels.nii')

%now write out R & L VAT data only within STN
STNhit=find(STNdata~=0);
catLSTNRSTN=cat(2,Lidentmatkeep,Ridentmatkeep);
sortcatLSTNRSTN=sort(catLSTNRSTN);

VATtempmatch = ismember(sortcatLSTNRSTN,STNhit);
LRVATreshapeallnonzero=cat(2,reshapeLVATallnonzero,reshapeLVATallnonzero);
LRVATreshapeallnonzero(:,VATtempmatch==0)=[];

LRVATonlySTN=LRVATreshapeallnonzero;

%write L R VAT data out as textfile

dlmwrite('LRVATreshapeonlySTN.txt',LRVATonlySTN,'delimiter','\t');

%determine most consistent voxels - potentially for mask in fsl randomise masking
thrperc=0.25;
thrreq=floor(thrperc*length(subFolders));

sumreshapeLVATall=sum(reshapeLVATallnonzero,1);
[~,LVATabvthrcol]=find(sumreshapeLVATall>=thrreq);

Lidentmatkeepabvthr=Lidentmatkeep(LVATabvthrcol);

LVATdataabvthr=zeros(size(LVATdata));
LVATdataabvthr(Lidentmatkeepabvthr)=1;
write(LVAThdr,LVATdataabvthr,'LVATdataabvthr.nii')

%density - across subjects
densreshapeLVAT=sumreshapeLVATall./(length(subFolders));
LVATdatadensity=zeros(size(LVATdata));

for i = 1:length(Lidentmatkeep)
LVATdatadensity(Lidentmatkeep(1,i))=densreshapeLVAT(1,i);  
end

nii = make_nii(LVATdatadensity, [0.5 0.5 0.5], [0 0 0], 64);
save_nii(nii, 'LVATdatadensity.nii');
end

