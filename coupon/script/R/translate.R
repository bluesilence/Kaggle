#################################################################################
# This script translates Japanese text to English in the data files
# and keeps the English translation in separate columns
#################################################################################
setwd('E:/Interest/kaggle/coupon/script')

# Create master translation table from Japanese to English
coupon_list_train = read.csv("../data/raw/coupon_list_train.csv", as.is=T) # Source file the English list is keyed by
trans = data.frame(
  jp=unique(c(coupon_list_train$GENRE_NAME, coupon_list_train$CAPSULE_TEXT,
              coupon_list_train$large_area_name, coupon_list_train$ken_name,
              coupon_list_train$small_area_name)),
  en=c("Food","Hair salon","Spa","Relaxation","Beauty","Nail and eye salon","Delivery service","Lesson","Gift card","Other coupon","Leisure","Hotel and Japanese hotel","Health and medical","Other","Hotel","Japanese hotel","Vacation rental","Lodge","Resort inn","Guest house","Japanse guest house","Public hotel","Beauty","Event","Web service","Class","Correspondence course","Kanto","Kansai","East Sea","Hokkaido","Kyushu-Okinawa","Northeast","Shikoku","Chugoku","Hokushinetsu","Saitama Prefecture","Chiba Prefecture","Tokyo","Kyoto","Aichi Prefecture","Kanagawa Prefecture","Fukuoka Prefecture","Tochigi Prefecture","Osaka prefecture","Miyagi Prefecture","Fukushima Prefecture","Oita Prefecture","Kochi Prefecture","Hiroshima Prefecture","Niigata Prefecture","Okayama Prefecture","Ehime Prefecture","Kagawa Prefecture","Tokushima Prefecture","Hyogo Prefecture","Gifu Prefecture","Miyazaki Prefecture","Nagasaki Prefecture","Ishikawa Prefecture","Yamagata Prefecture","Shizuoka Prefecture","Aomori Prefecture","Okinawa","Akita","Nagano Prefecture","Iwate Prefecture","Kumamoto Prefecture","Yamaguchi Prefecture","Saga Prefecture","Nara Prefecture","Mie","Gunma Prefecture","Wakayama Prefecture","Yamanashi Prefecture","Tottori Prefecture","Kagoshima prefecture","Fukui Prefecture","Shiga Prefecture","Toyama Prefecture","Shimane Prefecture","Ibaraki Prefecture","Saitama","Chiba","Shinjuku; Takadanobaba Nakano - Kichijoji","Kyoto","Ebisu; Meguro Shinagawa","Ginza Shinbashi; Tokyo; Ueno","Aichi","Kawasaki; Shonan-Hakone other","Fukuoka","Tochigi","Minami other","Shibuya; Aoyama; Jiyugaoka","Ikebukuro Kagurazaka-Akabane","Akasaka; Roppongi; Azabu","Yokohama","Miyagi","Fukushima","Much","Kochi","Tachikawa Machida; Hachioji other","Hiroshima","Niigata","Okayama","Ehime","Kagawa","Northern","Tokushima","Hyogo","Gifu","Miyazaki","Nagasaki","Ishikawa","Yamagata","Shizuoka","Aomori","Okinawa","Akita","Nagano","Iwate","Kumamoto","Yamaguchi","Saga","Nara","Triple","Gunma","Wakayama","Yamanashi","Tottori","Kagoshima","Fukui","Shiga","Toyama","Shimane","Ibaraki"),
  stringsAsFactors = F)

# Append data with translated columns...

# COUPON_LIST_TRAIN.CSV
coupon_list_train = read.csv("../data/raw/coupon_list_train.csv", as.is=T) # Read data file to translate
names(trans)=c("jp","en_capsule") # Rename column
coupon_list_train=merge(coupon_list_train,trans,by.x="CAPSULE_TEXT",by.y="jp",all.x=T) # Join translation onto original data
names(trans)=c("jp","en_genre");
coupon_list_train=merge(coupon_list_train,trans,by.x="GENRE_NAME",by.y="jp",all.x=T)
names(trans)=c("jp","en_small_area");
coupon_list_train=merge(coupon_list_train,trans,by.x="small_area_name",by.y="jp",all.x=T)
names(trans)=c("jp","en_ken");
coupon_list_train=merge(coupon_list_train,trans,by.x="ken_name",by.y="jp",all.x=T)
names(trans)=c("jp","en_large_area");
coupon_list_train=merge(coupon_list_train,trans,by.x="large_area_name",by.y="jp",all.x=T)
write.csv(coupon_list_train, "../data/intermediate/coupon_list_train_en.csv", row.names = F)

# COUPON_LIST_TEST.CSV
coupon_list_test = read.csv("../data/raw/coupon_list_test.csv", as.is=T) # Read data file to translate
names(trans)=c("jp","en_capsule") # Rename column
coupon_list_test=merge(coupon_list_test,trans,by.x="CAPSULE_TEXT",by.y="jp",all.x=T) # Join translation onto original data
names(trans)=c("jp","en_genre");
coupon_list_test=merge(coupon_list_test,trans,by.x="GENRE_NAME",by.y="jp",all.x=T)
names(trans)=c("jp","en_small_area");
coupon_list_test=merge(coupon_list_test,trans,by.x="small_area_name",by.y="jp",all.x=T)
names(trans)=c("jp","en_ken");
coupon_list_test=merge(coupon_list_test,trans,by.x="ken_name",by.y="jp",all.x=T)
names(trans)=c("jp","en_large_area");
coupon_list_test=merge(coupon_list_test,trans,by.x="large_area_name",by.y="jp",all.x=T)
write.csv(coupon_list_test, "../data/intermediate/coupon_list_test_en.csv", row.names = F)

# COUPON_AREA_TRAIN.CSV
coupon_area_train = read.csv("../data/raw/coupon_area_train.csv", as.is=T) 
names(trans)=c("jp","en_small_area");
coupon_area_train=merge(coupon_area_train,trans,by.x="SMALL_AREA_NAME",by.y="jp",all.x=T)
names(trans)=c("jp","en_pref");
coupon_area_train=merge(coupon_area_train,trans,by.x="PREF_NAME",by.y="jp",all.x=T)
write.csv(coupon_area_train, "../data/intermediate/coupon_area_train_en.csv", row.names = F)

# COUPON_AREA_TEST.CSV
coupon_area_test = read.csv("../data/raw/coupon_area_test.csv", as.is=T) 
names(trans)=c("jp","en_small_area");
coupon_area_test=merge(coupon_area_test,trans,by.x="SMALL_AREA_NAME",by.y="jp",all.x=T)
names(trans)=c("jp","en_pref");
coupon_area_test=merge(coupon_area_test,trans,by.x="PREF_NAME",by.y="jp",all.x=T)
write.csv(coupon_area_test, "../data/intermediate/coupon_area_test_en.csv", row.names = F)

# COUPON_DETAIL_TRAIN.CSV
coupon_detail_train = read.csv("../data/raw/coupon_detail_train.csv", as.is=T)
names(trans)=c("jp","en_small_area");
coupon_detail_train=merge(coupon_detail_train,trans,by.x="SMALL_AREA_NAME",by.y="jp",all.x=T)
write.csv(coupon_detail_train, "../data/intermediate/coupon_detail_train_en.csv", row.names = F)

# COUPON_VISIT_TRAIN.CSV
coupon_visit_train = read.csv("../data/raw/coupon_visit_train.csv", as.is=T)
# No jp columns in this table

# PREFECTURE_LOCATIONS.CSV
prefecture_locations = read.csv("../data/raw/prefecture_locations.csv", as.is=T)
names(prefecture_locations)=c("PREF_NAME","PREFECTUAL_OFFICE","LATITUDE","LONGITUDE")
names(trans)=c("jp","en_pref");
prefecture_locations=merge(prefecture_locations,trans,by.x="PREF_NAME",by.y="jp",all.x=T)
write.csv(prefecture_locations, "../data/intermediate/prefecture_locations_en.csv", row.names = F)

# USER_LIST.CSV
user_list = read.csv("../data/raw/user_list.csv", as.is=T)
names(trans)=c("jp","en_pref");
user_list=merge(user_list,trans,by.x="PREF_NAME",by.y="jp",all.x=T)
write.csv(user_list, "../data/intermediate/user_list_en.csv", row.names = F)
