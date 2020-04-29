; macro executing the 3D processing presented in Table 1
dim 3
itype 0
chsize 256 64 512
sin 0 F123
chsize % % (%*2)
rft f3
phase 90 90 F3
real F3
chsize % (%*2) %
rft f2
phase 90 90 F2
real F2
chsize (%*2) % %
rft f1
phase 90 90 F1
real F1
bcorr 1 2 F3 5 500 0


