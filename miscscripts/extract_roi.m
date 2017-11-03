function [loc, rois] = extract_roi(nii)

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
    rois{i}.centre = mean(rois{i}.coord);
end

for i = 1:length(rois)
    loc(i,:) = rois{i}.centre;
end

end
