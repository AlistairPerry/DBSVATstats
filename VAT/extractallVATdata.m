function extractallVATdata
%extractallVATdata Extract VAT voxels from each subject
%   Detailed explanation goes here

%required software dependencies:
%NBS

%make sure you are within /Lab_MichaelB/PhilM/VAT_Data_New

%setup working and subjects directories
workingdirectory = pwd;
files = dir(workingdirectory);
dirFlags=[files.isdir];
subFolders=files(dirFlags);
subFolders(1:2)=[];

%extract eg size of VAT images
demoRVATfile=[workingdirectory '/' '002' '/' 'rLEAD_DBS_VAT_RIGHT.nii'];
[VAThdr,VATdata]=read(demoRVATfile);
numvoxels=numel(VATdata);

%setup identity matrix for extracting VAT voxel information

VATmatdims=size(VATdata);
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
reshapeVATall=zeros(length(subFolders),numvoxels);

%load all subjects VATS and extract - start with R hemisphere for now
for s = 1:length(subFolders)
    currentSubj= subFolders(s,1).name;
    currentSubjDir = char([workingdirectory '/' currentSubj]);
    
    %parse VAT file string
    VATsubjids{s,1} = currentSubj;
    SubjRVATfile=[currentSubjDir '/' 'rLEAD_DBS_VAT_RIGHT.nii'];
    
    %load subject VAT
    [VAThdr,VATdata]=read(SubjRVATfile);
    %VATdataall(:,:,:,i)=VATdata;
    
    reshapeVATsubj=reshape(VATdata,[1 numvoxels]);
    reshapeVATall(s,:)=reshapeVATsubj;
end

%remove voxels with no VAT across all subjs
VATcolmax=max(reshapeVATall);
maxzero=find(VATcolmax==0);
[~,maxzero]=find(VATcolmax==0);
reshapeVATall(:,maxzero)=[];

%write VAT data out as textfile

dlmwrite('VATreshapeall.txt',reshapeVATall,'delimiter','\t');

fid = fopen(['VATsubijds.txt'], 'wt');
for s = 1:length(subFolders)
    fprintf(fid, '%s\n', VATsubjids{s,1});
end
fclose(fid)

% + voxels extracted
identmatkeep=1:numvoxels;
identmatkeep(:,maxzero)=[];

dlmwrite('VATdatavoxelinfo.txt',identmatkeep,'delimiter','\t');

%now as nii file for visualisationand masking purposes
VATdataallvoxels=zeros(size(VATdata));
VATdataallvoxels(identmatkeep)=1;
write(VAThdr,VATdataallvoxels,'VATdataallvoxels.nii')

%dlmwrite('VATdataallnew.txt',reshapeVATall,'delimiter','\t');
end

