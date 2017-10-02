function [VATstatsall] = calculateVATstats(STNparcdir)
%UNTITLED Summary of this function goes here
%   Detailed explanation goes here
%inputs

%required software dependencies:
%NBS

%make sure you are within /Lab_MichaelB/PhilM/VAT_Data_New (or another
%folder that contains leadDBS output files)

%setup working and subjects directories
workingdirectory = pwd;
files = dir(workingdirectory);
dirFlags=[files.isdir];
subFolders=files(dirFlags);
subFolders(1:2)=[];

%load in STN parcels

[STNmotorRhdr,STNmotorRdata]=read([STNparcdir '/' 'LEAD_DBS_STN_motor_RIGHT.nii']);

[STNmotorLhdr,STNmotorLdata]=read([STNparcdir '/' 'LEAD_DBS_STN_motor_LEFT.nii']);

[STNassocLhdr,STNassocLdata]=read([STNparcdir '/' 'LEAD_DBS_STN_associative_LEFT.nii']);

[STNassocRhdr,STNassocRdata]=read([STNparcdir '/' 'LEAD_DBS_STN_associative_RIGHT.nii']);

%extract their voxels
[STNmotorRvoxs]=find(STNmotorRdata==1);

[STNmotorLvoxs]=find(STNmotorLdata==1);

[STNassocRvoxs]=find(STNassocRdata==1);

[STNassocLvoxs]=find(STNassocLdata==1);

%load all subjects VATS and extract
for s = 1:length(subFolders)
    VATindivstats=[];
    
    currentSubj= subFolders(s,1).name;
    currentSubjDir = char([workingdirectory '/' currentSubj]);
    
    %parse VAT file strings
    VATsubjids{s,1} = currentSubj;
    SubjRVATfile=[currentSubjDir '/' 'rLEAD_DBS_VAT_RIGHT.nii'];
    [RVAThdr,RVATdata]=read(SubjRVATfile);
    
    SubjLVATfile=[currentSubjDir '/' 'rLEAD_DBS_VAT_LEFT.nii'];
    [LVAThdr,LVATdata]=read(SubjLVATfile);
    
    %calculate proportion of stimulation field in each STN zone
    [r1]=find(RVATdata==1);
    [l1]=find(LVATdata==1);
    
    %Right Motor
    
    cors=ismember(r1,STNmotorRvoxs);
    
    if isempty(cors)
        Rmotoroverlap=0;
        Rmotorperc=0;
    else
        Rmotoroverlap=find(cors==1);
        Rmotorperc=[length(Rmotoroverlap)./length(STNmotorRvoxs)]*100;
    end
    
    %Left Motor
    
    cors=ismember(l1,STNmotorLvoxs);
    
    if isempty(cors)
        Lmotoroverlap=0;
        Lmotorperc=0;
    else
        Lmotoroverlap=find(cors==1);
        Lmotorperc=[length(Lmotoroverlap)./length(STNmotorLvoxs)]*100;
    end
    
    %Right Assoc
    
    cors=ismember(r1,STNassocRvoxs);
    
    if isempty(cors)
        Rassocoverlap=0;
        Rassocperc=0;
    else
        Rassocoverlap=find(cors==1);
        Rassocperc=[length(Rassocoverlap)./length(STNassocRvoxs)]*100;
    end
    
    %Left Assoc
    
    cors=ismember(l1,STNassocLvoxs);
    
    if isempty(cors)
        Lassocoverlap=0;
        Lassocperc=0;
    else
        Lassocoverlap=find(cors==1);
        Lassocperc=[length(Lassocoverlap)./length(STNassocLvoxs)]*100;
    end
    
    %Right Limbic
    
    cors=ismember(r1,STNlimbicRvoxs);

    if isempty(cors)
    Rlimbicoverlap=0;
    Rlimbicperc=0;
    else
    Rlimbicoverlap=find(cors==1);
    Rlimbicperc=[length(Rlimbicoverlap)./length(STNlimbicRvoxs)]*100;
    end

    %Left Limbic

    cors=ismember(l1,STNlimbicLvoxs);

    if isempty(cors)
    Llimbicoverlap=0;
    Llimbicperc=0;
    else
    Llimbicoverlap=find(cors==1);
    Llimbicperc=[length(Llimbicoverlap)./length(STNlimbicLvoxs)]*100;
    end
    
%Combine individual VAT stats into single matrix    
VATindivstats=cat(2, Rmotorperc, Lmotorperc, Rassocperc, Lassocperc,Rlimbicperc,Llimbicperc);

%And then full subject matrix
VATstatsall(s,:)=VATindivstats;

end

%Now write VAT stats to output matrix

fid = fopen(['VATstats.txt'], 'wt');

fprintf(fid, '%s\t%s\t%s\t%s\t%s\n', 'ID', 'Rmotorperc','Lmotorperc','Rassocperc','Lassocperc','Rlimbicperc','Llimbicperc');
for s = 1:length(subFolders)
    fprintf(fid, '%s\t%f\t%f\t%f\t%f\n', VATsubjids{s,1},VATstatsall(s,1),VATstatsall(s,2),VATstatsall(s,3),VATstatsall(s,4),VATstatsall(s,5),VATstatsall(s,6));
end
fclose(fid)

%done!
end

