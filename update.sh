cd ./jxzhu_package
rm -r /home/jxzhu/.conda/envs/soap_ana/lib/python3.9/site-packages/jxzhu_package/*
cp -r `ls |grep -v .git|xargs` /home/jxzhu/.conda/envs/soap_ana/lib/python3.9/site-packages/jxzhu_package 
ssh -p 6666 -i ~/.ssh/id_rsa jxzhu@172.27.127.191 "rm -r /data/home/jxzhu/.conda/envs/soap/lib/python3.9/site-packages/jxzhu_package"
scp -r -P 6666 -i ~/.ssh/id_rsa /home/jxzhu/.conda/envs/soap_ana/lib/python3.9/site-packages/jxzhu_package jxzhu@172.27.127.191:/data/home/jxzhu/.conda/envs/soap/lib/python3.9/site-packages
git add .
git commit
git push
