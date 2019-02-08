
#The code creates a dataframe where the first columns is the x coordinate, y coordinate of the post code centroid and other attributes such as 
#the mean temperature, host number etc. The output file will be used by another R code to generate R0 in terms of postcode district rather than cells in the national grid

#

library(sp)
library(lattice) # required for trellis.par.set():
library(gstat, pos = match(paste("package", "sp", sep=":"), search()) + 1)
library(fields)
library(spam)
require(PBSmapping)
library(maptools)
library(rgeos)
library(stringi)
gpclibPermit()

##################################################################################################
################  Input shapefiles for Wales  ###############################################   

Wales_post_codes <- readShapeSpatial("Data_Base/Great_Britain/Wales_caspcs_2001/Wales_caspcs_2001_area.shp")
Scotland_post_codes <- readShapeSpatial("Data_Base/Great_Britain/Scotland_caspcs_2001/Scotland_caspcs_2001_area.shp")
England_post_codes <- readShapeSpatial("Data_Base/Great_Britain/England_caspcs_2001/England_caspcs_2001_area.shp")
##################################################################################################


################################################################################################
##  Simulated date after correction algorithm

##Opie_file<-"Data_Base/OPIE_data_base/report.csv"
Opie_file<-"Data_Base/OPIE_data_base/report.csv"

#livestock file

livestock_file<-"Data_Base/Host_data_base/livestock_DEFRA.txt"
grid_livestock_file<-"Data_Base/Host_data_base/grid_ref_en.csv"
#land use files
land_file_built_up<-"Data_base/Land_use/Built_up.dat"
land_file_open_water<-"Data_base/Land_use/open_water.dat"

#Temperature files

series1<-paste("Data_base/Climate_data_base/MeanTemp_2001-2006/MeanTemp_2006-0", 1:9,"_Actual.txt", sep = "")
series2<-paste("Data_base/Climate_data_base/MeanTemp_2001-2006/MeanTemp_2006-", 10:12,"_Actual.txt", sep = "")
month_data <- c(series1,series2)

pre<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
series1<-paste("Data_base/Climate_data_base/MinTemp_1961-1990_LTA/MinTemp_",pre, "_Average_Actual.txt",  sep = "")
min_month_data <- c(series1)

series1<-paste("Data_base/Climate_data_base/MaxTemp_1961-1990_LTA/MaxTemp_",pre, "_Average_Actual.txt",  sep = "")
max_month_data <- c(series1)


month<-c("January","February","March","April","May","June","July","August","September","October","November","December")
i_month<-7
# it does not matter which month

################################################################################################


################################################################################################
##  Read infection occurrences


# Opie_data
Opie_data<-read.csv(Opie_file)
colnames(Opie_data)<-c("ID","name","Sex","County","PC","date")



################################################################################################
##  Limit to cases when post code is known

ind_pc<-which(as.character(Opie_data$PC)=="" |as.character(Opie_data$PC)==" " | as.character(Opie_data$PC)=="   ")
Opie_data<-Opie_data[-ind_pc,] # Restricting to patients with PC only

Opie_data$PC<-stri_replace_all_fixed(as.character(Opie_data$PC), " ", "") # Remove white spaces from a string


substrLeft <- function(x, n){
  #substr(x, nchar(x)-n+1, nchar(x))
  substr(x, 1, nchar(x)-n)
}  # Function to remove the last two characters from post code sector (we are only intersted in post code district)


substrCorr <- function(x){
  #substr(x, nchar(x)-n+1, nchar(x))
  
  st1<-substr(x, 1, nchar(x)-2)
  #    st2<-" "
  st3<-substr(x, nchar(x)-2, nchar(x)-2)
  paste(st1,st3)
  
}  # Function to )

diff_string<-nchar(Opie_data$PC)-5

Opie_data$PC<-substrLeft(Opie_data$PC, diff_string)



################################################################################################
##  Limit to particular years

Opie_date<-as.Date(Opie_data$date, "%d/%m/%Y")

ind_date<-which(as.numeric(format(Opie_date, "%Y"))=="2006")
Opie_data<-Opie_data[ind_date,] # Restricting to particular years

ind_month<-unique(as.numeric(format(as.Date(Opie_data$date, "%d/%m/%Y"), "%m")))
ind_year<-unique(as.numeric(format(as.Date(Opie_data$date, "%d/%m/%Y"), "%Y")))






################################################################################################

##  Read average temperature from csv data from met office. Data are presentated on a national grid

#temperature_temporal_data_base<-function(week,year){}
temperature_temporal_data_base<-function(ind_month,ind_year){
temperature <- read.csv(file=month_data[ind_month],col.names=paste("X", 1:181, sep = ""),row.names=paste("Y", 1:289, sep = ""),sep=" ",na.strings = "-9999",skip=6)  #Need to sort out year and week
# the first rows provide the following information
ncols <-        180
nrows <-        290
xllcorner<-     -200000
yllcorner<-     -200000
cellsize<-      5000

#Create a dataframe where the first columns is the x coordinate, y coordinate and other attributes such as the mean temperature
## now create a grid to a grid with 5000 meter spacing:

tot_rows<-nrows*ncols

x_grid<-matrix(1,tot_rows)
y_grid<-matrix(1,tot_rows)
temperature_grid<- matrix(1,tot_rows)

for (i_row in c(1:nrows))
{
    for (i_col in c(1:ncols))
    
    {
        
        #x_grid[(i_col+(i_row-1)*ncols)]<-xllcorner+(i_col)*cellsize
        #y_grid[(i_col+(i_row-1)*ncols)]<-yllcorner+(nrows+1-i_row)*cellsize #WARNING CHECK THIS!!!
        temperature_grid[i_col+(i_row-1)*ncols]<- temperature[i_row,i_col]
        
    }
}


##  Read max (averaged over many years) temperature from csv data from met office. Data are presentated on a national grid


temperature_max <- read.csv(file=max_month_data[i_month],col.names=paste("X", 1:181, sep = ""),row.names=paste("Y", 1:289, sep = ""),sep=" ",na.strings = "-9999",skip=6)
# the first rows provide the following information

temperature_grid_max<- matrix(1,tot_rows)

for (i_row in c(1:nrows))
{
    for (i_col in c(1:ncols))
    
    {
        
        temperature_grid_max[i_col+(i_row-1)*ncols]<- temperature_max[i_row,i_col]
        
    }
}


grd <- GridTopology(cellcentre.offset=c(xllcorner,yllcorner), cellsize=c(cellsize,cellsize), cells.dim=c(ncols,nrows))

Sp_grid<-SpatialGrid(grid = grd)


SpP_grd <- as.SpatialPolygons.GridTopology(grd)

SpP_points <- as.SpatialPolygons.GridTopology(grd)

trdata <- data.frame(temperature_grid, row.names=sapply(slot(SpP_grd, "polygons"), function(i) slot(i, "ID")))
trdata2 <- data.frame(temperature_grid)

SpPDF <- SpatialPolygonsDataFrame(SpP_grd, trdata)

Spgrid_df <- SpatialGridDataFrame(Sp_grid, trdata2)
t_mean<-mean(na.omit(temperature_grid))

Spgrid_df_list <- list(ind_year,ind_month,Spgrid_df,t_mean)
}



######################################################################################################
##################################   intersection with shape files  ##################################

############################# Animal density and other attribute  Wales ##############################






################################################################################################
##  Read land_use file. Data are presentated according to coordinates

land_built_up <- read.csv(land_file_built_up,sep=" ",skip = 2)
colnames(land_built_up)<-c("x_land","y_land","value_land")


land_built_up$X_land<-land_built_up$x_land*1000
land_built_up$Y_land<-land_built_up$y_land*1000
xy_land_built_up <- cbind(land_built_up$X_land, land_built_up$Y_land)
xy_land_built_up.sp <- SpatialPoints(xy_land_built_up)
df_land_built_up <- data.frame(bu = land_built_up$value_land)
xy_land_built_up.spdf <- SpatialPointsDataFrame(xy_land_built_up.sp, df_land_built_up)


################################################################################################





################################################################################################
##  Read cattle sheep and pigs population from DEFRA data. Data are presentated according to national grid (may be shifted compared to the met-office one)

livestock <- read.csv(livestock_file,sep="\t")
colnames(livestock )<-c("Position","cattle","sheep_w","sheep_june","pig")
grid_livestock<- read.csv(grid_livestock_file)
colnames(grid_livestock)<-c("x_livestock","y_livestock")


xy_livestock <- cbind(grid_livestock$x_livestock, grid_livestock$y_livestock)


xy_livestock.sp <- SpatialPoints(xy_livestock)
df_cattle <- data.frame(livestock$cattle)
df_sheep_w <- data.frame(livestock$sheep_w)

# this is a SpatialPoints class with attributes, the attribute is the number of animals on each postcode

xy_cattle.spdf <- SpatialPointsDataFrame(xy_livestock.sp, df_cattle)
xy_sheep_w.spdf <- SpatialPointsDataFrame(xy_livestock.sp, df_sheep_w)

################################################################################################




################ link temperature to patient location #############



Wales_post_codes.sp <- as(Wales_post_codes, "SpatialPolygons")
Scotland_post_codes.sp <- as(Scotland_post_codes, "SpatialPolygons")
England_post_codes.sp <- as(England_post_codes, "SpatialPolygons")





################################################################################################




################################################################################################


################ Function to deal with list of 0 elements
sum_function_cattle<-function(x)
{
	if(length(x$livestock.cattle)==0) 
	{
		
	    ans<-0
	}
	else
	{
		sum(x)
	}
}

sum_function_sheep<-function(x)
{
	if(length(x$livestock.sheep_w)==0) 
	{
		ans<-0
	}
	else
	{
		sum(x)
	}
}


mean_temp<-function(x)
{
    if(length(x$temperature_grid)==0)
    {
        ans<-0
    }
    else
    {
        mean(x[[]]$temperature_grid)
    }
}

################


cattle_in_Wales <- over(Wales_post_codes.sp,xy_cattle.spdf,returnList=TRUE)
sum_cattle_Wales<-unlist(lapply(cattle_in_Wales,sum_function_cattle))

sheep_in_Wales <- over(Wales_post_codes.sp,xy_sheep_w.spdf,returnList=TRUE)
sum_sheep_Wales<-unlist(lapply(sheep_in_Wales,sum_function_sheep))


temp_in_Wales<-cbind()
temperature_in_Wales<- over(Wales_post_codes.sp,Spgrid_df,returnList=TRUE)
#mean_temperature_in_Wales<- unlist(lapply(temperature_in_Wales,mean_temp))




bi_in_Wales<-cbind()
built_in_Wales<- over(Wales_post_codes.sp,xy_land_built_up.spdf,returnList=TRUE)
#mean_built_in_Wales<- unlist(lapply(built_in_Wales,mean))

Wales_areas<-cbind()

Opie_in_Wales<-cbind()










for(i in c(1:length(Wales_post_codes.sp)))
{
    temp_in_Wales[i]<-mean(temperature_in_Wales[[i]]$temperature_grid)
    bi_in_Wales[i]<-mean(built_in_Wales[[i]]$bu)
    Wales_areas[i]<-Wales_post_codes.sp@polygons[[i]]@area
    
    
    Opie_in<-which(as.character(Wales_post_codes$LABEL)==Opie_data$PC[i])
    Opie_in_corrected<-which(as.character(Wales_post_codes$LABEL)==substrCorr(Opie_data$PC[i]))
    
    if(length(Opie_in)!=0)
    {
        Opie_in_Wales[i]<-Opie_in}
    else{
        
        if(length(Opie_in_corrected)!=0)
        {
            Opie_in_Wales[i]<-which(as.character(Wales_post_codes$LABEL)==substrCorr(Opie_data$PC[i]))
        } else {
            Opie_in_Wales[i]<-NA
        }
        
    }


    

}

indx<-cbind(c(1:length(Wales_post_codes.sp)),temp_in_Wales)
indx_t<-indx[complete.cases(indx),]
indx_f<-indx[!complete.cases(indx),]

indb<-cbind(c(1:length(Wales_post_codes.sp)),bi_in_Wales)
indb_t<-indb[complete.cases(indb),]
indb_f<-indb[!complete.cases(indb),]

#x_cent<-cbind()
#y_cent<-cbind()



for(i in as.vector(indb_f[,1]))
{
    
        
    if(is.na(bi_in_Wales[i]))
    {
       
        bi_in_Wales[i]<-0# ths because some small postcode near  the sea where T=NA in the cell
        
    }
    
    #print(bi_in_Wales[i])
    #print(i)
    
    #plot(Wales_post_codes.sp[i],col="magenta")
    #points(centre_grid.spdf,col="blue")
    #Sys.sleep(1)
    
}




Opie_in_Wales<-Opie_in_Wales[!is.na(Opie_in_Wales)]

#x_cent<-cbind()
#y_cent<-cbind()
#for(i in as.vector(indx_f[,1]))
for(i in as.vector(Opie_in_Wales)) {
    
    #   x_cent[i]<-gCentroid(Wales_post_codes.sp[i], byid=FALSE, id = NULL)@coords[1]
    #y_cent[i]<-gCentroid(Wales_post_codes.sp[i], byid=FALSE, id = NULL)@coords[2]
    tt<-over(SpPDF,gCentroid(Wales_post_codes.sp[i], byid=FALSE, id = NULL),returnList=FALSE)
    temp_in_Wales[i]<-temperature_grid[( which(tt==TRUE))]
       
    if(is.na(temp_in_Wales[i]))
    {
        temp_in_Wales[i]<-t_mean# ths because some small postcode near  the sea where T=NA in the cell
               
    }
    
    #print(temp_in_Wales[i])
    #print(i)
    
    #plot(Wales_post_codes.sp[i],col="magenta")
    #points(centre_grid.spdf,col="blue")
    #Sys.sleep(1)
    
}









#######################




###### Information regarding Scotland  ##############################

Scotland_post_codes.sp <- as(Scotland_post_codes, "SpatialPolygons")



cattle_in_Scotland <- over(Scotland_post_codes.sp,xy_cattle.spdf,returnList=TRUE)
sum_cattle_Scotland<-unlist(lapply(cattle_in_Scotland,sum_function_cattle))

sheep_in_Scotland <- over(Scotland_post_codes.sp,xy_sheep_w.spdf,returnList=TRUE)
sum_sheep_Scotland<-unlist(lapply(sheep_in_Scotland,sum_function_sheep))


temp_in_Scotland<-cbind()
temperature_in_Scotland<- over(Scotland_post_codes.sp,Spgrid_df,returnList=TRUE)
#mean_temperature_in_Scotland<- unlist(lapply(temperature_in_Scotland,mean))




bi_in_Scotland<-cbind()
built_in_Scotland<- over(Scotland_post_codes.sp,xy_land_built_up.spdf,returnList=TRUE)
#mean_built_in_Scotland<- unlist(lapply(built_in_Scotland,mean))


Scotland_areas<-cbind()
Opie_in_Scotland<-cbind()


for(i in c(1:length(Scotland_post_codes.sp)))
{
    temp_in_Scotland[i]<-mean(temperature_in_Scotland[[i]]$temperature_grid)
    bi_in_Scotland[i]<-mean(built_in_Scotland[[i]]$bu)
    Scotland_areas[i]<-Scotland_post_codes.sp@polygons[[i]]@area
    
    Opie_in<-which(as.character(Scotland_post_codes$LABEL)==Opie_data$PC[i])
    Opie_in_corrected<-which(as.character(Scotland_post_codes$LABEL)==substrCorr(Opie_data$PC[i]))
    
    if(length(Opie_in)!=0)
    {
        Opie_in_Scotland[i]<-Opie_in}
    else{
        
        if(length(Opie_in_corrected)!=0)
        {
            Opie_in_Scotland[i]<-which(as.character(Scotland_post_codes$LABEL)==substrCorr(Opie_data$PC[i]))
        } else {
            Opie_in_Scotland[i]<-NA
        }
        
    }
    
}


indx<-cbind(c(1:length(Scotland_post_codes.sp)),temp_in_Scotland)
indx_t<-indx[complete.cases(indx),]
indx_f<-indx[!complete.cases(indx),]

indb<-cbind(c(1:length(Scotland_post_codes.sp)),bi_in_Scotland)
indb_t<-indb[complete.cases(indb),]
indb_f<-indb[!complete.cases(indb),]

#x_cent<-cbind()


for(i in as.vector(indb_f[,1]))
{
    
    
    if(is.na(bi_in_Scotland[i]))
    {
        
        bi_in_Scotland[i]<-0# ths because some small postcode near  the sea where T=NA in the cell
        
    }
    
    #print(bi_in_Scotland[i])
    # print(i)
    
    #plot(Wales_post_codes.sp[i],col="magenta")
    #points(centre_grid.spdf,col="blue")
    #Sys.sleep(1)
    
}





#x_cent<-cbind()
#y_cent<-cbind()
Opie_in_Scotland<-Opie_in_Scotland[!is.na(Opie_in_Scotland)]


for(i in seq(1: length(Opie_in_Scotland)))
{
    
    #x_cent[i]<-gCentroid(Scotland_post_codes.sp[i], byid=FALSE, id = NULL)@coords[1]
    #y_cent[i]<-gCentroid(Scotland_post_codes.sp[i], byid=FALSE, id = NULL)@coords[2]
    tt<-over(SpPDF,gCentroid(Scotland_post_codes.sp[i], byid=FALSE, id = NULL),returnList=FALSE)
    temp_in_Scotland[i]<-temperature_grid[( which(tt==TRUE))]
       
    if(is.na(temp_in_Scotland[i]))
    {
        temp_in_Scotland[i]<-t_mean# ths because some small postcode near  the sea where T=NA in the cell
        
    }
    
    #print(temp_in_Scotland[i])
   
   # print(i)
    
    #plot(Scotland_post_codes.sp[i],col="magenta")
    #points(centre_grid.spdf,col="blue")
    #Sys.sleep(1)
    
}





#########################################################################################


England_post_codes.sp <- as(England_post_codes, "SpatialPolygons")



cattle_in_England <- over(England_post_codes.sp,xy_cattle.spdf,returnList=TRUE)
sum_cattle_England<-unlist(lapply(cattle_in_England,sum_function_cattle))

sheep_in_England <- over(England_post_codes.sp,xy_sheep_w.spdf,returnList=TRUE)
sum_sheep_England<-unlist(lapply(sheep_in_England,sum_function_sheep))


temp_in_England<-cbind()
temperature_in_England<- over(England_post_codes.sp,Spgrid_df,returnList=TRUE)
#mean_temperature_in_England<- unlist(lapply(temperature_in_England,mean))




bi_in_England<-cbind()
built_in_England<- over(England_post_codes.sp,xy_land_built_up.spdf,returnList=TRUE)
#mean_built_in_England<- unlist(lapply(built_in_England,mean))


England_areas<-cbind()
Opie_in_England<-cbind()

for(i in c(1:length(England_post_codes.sp)))
{
    
    
    temp_in_England[i]<-mean(temperature_in_England[[i]]$temperature_grid)
    bi_in_England[i]<-mean(built_in_England[[i]]$bu)
    England_areas[i]<-England_post_codes.sp@polygons[[i]]@area
   
    Opie_in_England[i]<-NA
   
    Opie_in<-which(as.character(England_post_codes$LABEL)==Opie_data$PC[i])
    Opie_in_corrected<-which(as.character(England_post_codes$LABEL)==substrCorr(Opie_data$PC[i]))
    
    if(length(Opie_in)!=0)
    {
        Opie_in_England[i]<-Opie_in}
    else{
        
        if(length(Opie_in_corrected)!=0)
        {
     Opie_in_England[i]<-which(as.character(England_post_codes$LABEL)==substrCorr(Opie_data$PC[i]))
        } else {
        Opie_in_England[i]<-NA
        }
    
    }
    
}




indx<-cbind(c(1:length(England_post_codes.sp)),temp_in_England)
indx_t<-indx[complete.cases(indx),]
indx_f<-indx[!complete.cases(indx),]

indb<-cbind(c(1:length(England_post_codes.sp)),bi_in_England)
indb_t<-indb[complete.cases(indb),]
indb_f<-indb[!complete.cases(indb),]

#x_cent<-cbind()


for(i in as.vector(indb_f[,1]))
{
    
    
    if(is.na(bi_in_England[i]))
    {
        
        bi_in_England[i]<-0# ths because some small postcode near  the sea where T=NA in the cell
        
    }
    
    #print(bi_in_Scotland[i])
    # print(i)
    
    #plot(Wales_post_codes.sp[i],col="magenta")
    #points(centre_grid.spdf,col="blue")
    #Sys.sleep(1)
    
}



#for(i in seq(1:length(Opie_in_England)))
#{
    
    
#   if(is.na(bi_in_England[Opie_in_England[i]]))
#   {
        
#       bi_in_England[Opie_in_England[i]]<-0# ths because some small postcode near  the sea where T=NA in the cell
        
#   }
    
    #print(bi_in_England[i])
    # print(i)
    
    #plot(Wales_post_codes.sp[i],col="magenta")
    #points(centre_grid.spdf,col="blue")
    #Sys.sleep(1)
    
#}


#x_cent<-cbind()
#y_cent<-cbind()
Opie_in_England<-Opie_in_England[!is.na(Opie_in_England)]

for(i in seq(1:length(Opie_in_England)))
{
    
    #x_cent[i]<-gCentroid(England_post_codes.sp[i], byid=FALSE, id = NULL)@coords[1]
    #y_cent[i]<-gCentroid(England_post_codes.sp[i], byid=FALSE, id = NULL)@coords[2]
    tt<-over(SpPDF,gCentroid(England_post_codes.sp[Opie_in_England[i]], byid=FALSE, id = NULL),returnList=FALSE)
    temp_in_England[Opie_in_England[i]]<-temperature_grid[( which(tt==TRUE))]
       
    if(is.na(temp_in_England[Opie_in_England[i]]))
    {
        temp_in_England[Opie_in_England[i]]<-t_mean# ths because some small postcode near  the sea where T=NA in the cell
            
    }
    
    #print(temp_in_England[Opie_in_England[i]])
    #print(i)
    #print(Opie_in_England[i])
    
    #plot(England_post_codes.sp[i],col="magenta")
    #points(centre_grid.spdf,col="blue")
    #Sys.sleep(1)
    
}

GBR_df<-c()
if(length(Opie_in_Wales)!=0){
    indW<-seq(1:length(Opie_in_Wales))
Wales_df<-data.frame(Wales_post_codes$LABEL[indW],Wales_areas[indW],sum_cattle_Wales[indW],sum_sheep_Wales[indW],
    temp_in_Wales[indW],bi_in_Wales[indW])
    GBR_df<-rbind(GBR_df,Wales_df)


}

if(length(Opie_in_Scotland)!=0){
    indS<-seq(1:length(Opie_in_Scotland))
    Scotland_df<-data.frame(Scotland_post_codes$LABEL[indS],Scotland_areas[indS],sum_cattle_Scotland[indS],sum_sheep_Scotland[indS],
    temp_in_Scotland[indS],bi_in_Scotland[indS])
    GBR_df<-rbind(GBR_df,Scotland_df)
}

if(length(Opie_in_England)!=0){
indE<-seq(1:length(Opie_in_England))

England_df<-data.frame(England_post_codes$LABEL[indE],England_areas[indE],sum_cattle_England[indE],sum_sheep_England[indE],
temp_in_England[indE],bi_in_England[indE])
GBR_df<-rbind(GBR_df,England_df)
}

colnames(GBR_df)<-c("PostCode","Area","Cattle","Sheep","Temp","Built-up")



###################################### end shape files ###################################################

df_temp_clean<- na.omit(GBR_df)


#png(filename = graph_temperature[i_month],width = 980, height = 980, units = "px",pointsize = 16, bg = "white")

zmin<-min(na.omit(temperature_grid))
zmax<-max(na.omit(temperature_grid))

image(df_temp, col=topo.colors(64),axes = FALSE)
image.plot( zlim=c(zmin,zmax), nlevel=64,legend.only=TRUE, horizontal=TRUE,
col=topo.colors(64),add=TRUE)
#points(xy.spdf,col=xy.spdf$z1,cex = 0.15,pch = 20)
#c_vec<-seq(1,20,by=2)
#legend(0, 0, paste(1:10), col=c_vec,  pch = 20, ncol = 1, cex = 0.8,title.col ="No Horses per cell")



title(main=paste("Temperature  distribution:", month[i_month]))


dev.off()


write.table(GBR_df)
write.table(GBR_df, file = prepared_data[i_month], quote = FALSE, sep = ",")

#ramp <- colorRamp(c("red", "white"))
#plot_colors<-rgb( ramp(seq(0, 1, length = 481)), max = 255)

#library(RColorBrewer)
#xx<-Wales_df$sum_horses_Wales/Wales_df$Wales_areas

#plot_colors<-c(rgb(xx/max(xx),0,0, max = 1))

#plot(Wales_post_codes.sp,border="white")
#for(i in as.vector(indx_f[,1]))
#{

#plot(Wales_post_codes.sp[i],col=as.numeric(xx[i]/xx_m),add=TRUE,border=as.numeric(xx[i]/xx_m))

#}
#for(i in as.vector(indx_t[,1]))
#{
#   plot(Wales_post_codes.sp[i],col="magenta",add=TRUE)
#}
#points(centre_grid.spdf,col="blue")
# over is not always working!!!






