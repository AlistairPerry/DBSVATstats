function [COG, rois] = extract_roi(nii)
%% Extract centre of gravity for each integer within a NIFTI file

%% Input:
%%% nii: Filename of input nii file

%% Output
%%% COG: Centre of gravity for each integer i, denoted by a i x 3 structure
%%% rois: Structure corresponding to one fields; 
%%%% 1) coord: coordinates for each voxel within each integer

%% Dependencies:
%%% NIFTI tools: https://de.mathworks.com/matlabcentral/fileexchange/8797-tools-for-nifti-and-analyze-image

nifti = load_untouch_nii(nii);

rois = {};
for i = 1:nifti.hdr.dime.dim(2)
    for j = 1:nifti.hdr.dime.dim(3)
        for k = 1:nifti.hdr.dime.dim(4)
            if nifti.img(i,j,k) > 0
                try
                    rois{nifti.img(i,j,k)}.coord = vertcat(rois{nifti.img(i,j,k)}.coord,[i-1,j-1,k-1]);
                catch
                    rois{nifti.img(i,j,k)}.coord = [i-1,j-1,k-1];
                end
            end
        end
    end
end

for i = 1:length(rois)
    rois{i}.coord(:,1) = (rois{i}.coord(:,1)*nifti.hdr.hist.srow_x(1)) + nifti.hdr.hist.srow_x(4);
    rois{i}.coord(:,2) = (rois{i}.coord(:,2)*nifti.hdr.hist.srow_y(2)) + nifti.hdr.hist.srow_y(4);
    rois{i}.coord(:,3) = (rois{i}.coord(:,3)*nifti.hdr.hist.srow_z(3)) + nifti.hdr.hist.srow_z(4);
    COG(i,:) = mean(rois{i}.coord);
end

end
