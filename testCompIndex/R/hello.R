# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

hello = function(data, x1, l, ld, sss) {
  # data：输入数据
  # x1：年份
  # l：判断是两栖还是鸟类的类别
  print("指数开始计算...")
  sss = as.numeric(sss)
  x1 = as.numeric(x1)
  data3 = unique(data)

  N = 20 # 随机采样的次数
  z = array(0, dim = c(length(x1), 6, N))
  #line=unique(data3$样线编码)
  ####建立存储随机指数的矩阵
  for (de in 1:N) {
    line = unique(data3$样线编码)
    #################选出观测年都有的样线
    #for(i in 1:length(x1)){
    #  data1=sub=subset(data3,data3$年份==x1[i]&data3$样线编码%in%line)
    #  line=unique(data1$样线编码)
    #}
    set.seed(de)
    line = sample(line, size = round(0.6 * as.numeric(length(line))))
    ##################？内存area#################
    area = read.csv(file = paste0(ld, "area.csv"), fileEncoding = 'gbk')
    #############################计算数量,输出abc[y]############
    abc = list()
    for (y in 1:length(x1)) {
      data = subset(data3, data3$年份 == x1[y] & data3$样线编码 %in% line)
      if (l == c("两栖")) {
        data = data[c("年份", "物种名", "长度", "宽度", "省份", "物种数量")]
      }
      if (l == c("鸟类")) {
        data = data[c("年份", "物种名", "长度", "省份", "物种数量")]
      }
      data = na.omit(data)
      names = unique(data$物种名)
      names = as.matrix(names)


      #######建立存储数量的矩阵###########
      ###########所有物种的数量###########
      num = matrix(NA, nrow = length(names), ncol = 2)
      #########按物种分类
      for (i in 1:length(names)) {
        species = subset(data, 物种名 == names[i])
        #########选取省份########
        provi = unique(species$省份)
        provi = as.matrix(provi)
        pro_num0 = NA
        if (sss != -1) {
          if (l == c("两栖")) {
            densi = sum(species$物种数量) / sum(species$长度 * species$宽度)
          }
          if (l == c("鸟类")) {
            densi = sum(species$物种数量) / sum(species$长度 * 200)
          }

          ########################计算面积###########################
          # pro_num=subset(area,province==provi[j])$aera*densi*1000
          num[i, 1] = as.matrix(names[i])
          num[i, 2] = ceiling(densi * sss)
        } else{
          for (j in 1:length(provi)) {
            ######################if 是两栖##############
            if (l == c("两栖")) {
              species2 = subset(species, 省份 == provi[j])
              densi = sum(species2$物种数量) / sum(species2$长度 * species2$宽度)
            }
            if (l == c("鸟类")) {
              species2 = subset(species, 省份 == provi[j])
              densi = sum(species2$物种数量) / sum(species2$长度 * 200)
            }

            ########################计算面积###########################
            # pro_num=subset(area,province==provi[j])$aera*densi*1000
            pro_num = subset(area, province == provi[j])$aera * densi * 1000
            pro_num0 = rbind(pro_num, pro_num0)
          }
          num[i, 1] = as.matrix(names[i])
          num[i, 2] = ceiling(sum(as.numeric(pro_num0[, 1]), na.rm = TRUE))
        }
      }
      #######################输出该年计算得到的数量#####################
      # file1=paste(x1[y],"num.csv")
      # write.csv(num,file1)
      abc[[y]] = num
      # ll = paste0(ld, "index/")
      # ll = paste0(ll, l)
      # ll = paste0(ll, x1[y])
      # ll = paste0(ll, "年物种个体数.csv")
      # write.csv(num, ll)
    }

    exp_other = matrix(0, ncol = 6, nrow = length(x1))
    colnames(exp_other) <- c("year", "ent", "s", "logn", "改进1", "改进2")

    exp_other = as.data.frame(exp_other)
    ################读取文件#############

    for (y in 1:length(x1)) {
      data = abc[[y]]
      data = as.data.frame(data)
      n = sum(as.numeric(as.character(data$V2)))
      exp_other$logn[y] = log(n, 10)
      p = as.numeric(as.character(data$V2)) / n
      a = sum(log(p, 10)) / length(p) + log(n, 10)
      exp_other$year[y] = x1[y]
      exp_other$ent[y] = sum(-p * log(p))
      exp_other$s[y] = length(p)
      exp_other$改进1[y] = (sum(-p * log(p)) + log(n, 10))
      exp_other$改进2[y] = (a + sum(-p * log(p)))
    }
    # print(nrow(exp_other))
    # print(exp_other["改进1"])
    # print(class(exp_other))
    ##################输出exp_other######################
    print(exp_other)
    z[, , de] = as.matrix(exp_other)
    #incProgress(1 / N)
    print(de)
  }

  cnt = 0
  for(i in 1:N){
    if( sum(is.na(z[,,i]))==0 ){
      cnt = cnt+1
    }
  }
  z1 = array(0, dim = c(length(x1), 6, cnt))
  j = 1
  for(i in 1:N){
    if( sum(is.na(z[,,i]))==0 ){
      z1[,,j] = z[,,i]
      j = j+1
    }
  }
  z = z1

  ###计算均值和置信区间
  all = array(0, dim = c(length(x1), 8))
  yiup = NULL
  yidown = NULL
  erup = NULL
  erdown = NULL
  for (c in 1:length(x1)) {
    for (b in 1:6) {
      if (b == 3) {
        all[c, b] = as.integer(mean(z[c, b, ]))
      } else{
        all[c, b] = round(mean(z[c, b, ]), 4)
      }
    }
    yi = t.test(z[c, 5, ])
    all[c, 7] = paste(round(yi$conf.int[1], 2), round(yi$conf.int[2], 2), sep =
                        "——")
    er = t.test(z[c, 6, ])
    all[c, 8] = paste(round(er$conf.int[1], 2), round(er$conf.int[2], 2), sep =
                        "——")
    yiup = cbind(round(yi$conf.int[1], 2), yiup)
    yidown = cbind(round(yi$conf.int[2], 2), yidown)
    erup = cbind(round(er$conf.int[1], 2), erup)
    erdown = cbind(round(er$conf.int[2], 2), erdown)
  }


  colnames(all) <-
    c("year", "ent", "s", "logn", "改进1", "改进2", "改进1置信区间", "改进2置信区间")
  res = list()
  res[[1]] = all
  res[[2]] = yiup
  res[[3]] = yidown
  res[[4]] = erup
  res[[5]] = erdown

  print("指数计算完成！")
  return(res)
}
