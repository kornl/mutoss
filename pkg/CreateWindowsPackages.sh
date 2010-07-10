cd mutossGUI/
ant

cd ..
R CMD build mutoss
mv mutoss_0.1-0.tar.gz ../releasedPackages/

R CMD build mutossGUI
mv mutossGUI_0.0-1.tar.gz ../releasedPackages/

cd ../releasedPackages

R CMD INSTALL --build mutoss_0.1-0.tar.gz
rm -rf mutoss
tar -xzf mutoss_0.1-0_R_x86_64-pc-linux-gnu.tar.gz
zip -r mutoss_0.1-0.zip mutoss

R CMD INSTALL --build mutossGUI_0.0-1.tar.gz
rm -rf mutossGUI
tar -xzf mutossGUI_0.0-1_R_x86_64-pc-linux-gnu.tar.gz
zip -r mutossGUI_0.0-1.zip mutossGUI

