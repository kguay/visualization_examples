# satellite_image_inventory.py
# 
# Created by: Kevin Guay
# Modified:   07-05-12
# 
# This python script searches through the directories specified in the 'roots' table and indexes
# 	all of the spatially referenced raster files that it finds.  
#
# USEAGE:	python inv.py -pram
#	-p: Print messages to terminal
#	-r: Search recursively
#	-a: Search all roots
#	-m: Only inventory directories that contain metadata (meta.txt)
#
# UPDATES:
#	7-05	Modified to use GeoID version 0.1 (instead of borealdb)
#	7-11	Modified to use GeoID version 0.2


import sys, os, pg, glob, geomops, geoutil
from osgeo import gdal, osr
from gdalconst import * 

# Directories (must end in a '/')
ROOT_DIR		= "/home/kguay/dev/raw/"		# Root directory for search /mnt/boreal/e/AK_summer_WELDv1.5
ROOT			= "/home/kguay/dev/"	# Root directory for folders below
WORK_DIR 		= ROOT + "working/"


# Database Information
DB_HOST			= "arctic.whrc.ad"
DB_NAME			= "geoid"
DB_USER			= "kguay"		# password stored in .pgpwd file
db				=  None			# will be set by connect_db() function

							
# Constants: TLO = "Top Left lOngitude", TLA = "Top Left lAtitude
TLO	= 0;TLA	= 1;TRO	= 4;TRA	= 5;BRO	= 6;BRA	= 7;BLO	= 2;BLA = 3

# A list of file extentions that are accepted by the program
FILE_TYPES  = ('', '.hdf', '.tif', '.TIF', '.asc', '.img')
	
# A directory to hold thumbnail previews of the images
THUMB_DIR		= "/home/kguay/thumb/"

print2terminal = False; write2file = False; prog2terminal = False
srec = False; sall = False; output_file = None; onlyMeta = False

##################################### DEBUG PRINTING #####################################

# _print(string): Lets the user control if/ where he sees the program output
#
# print_str: a string to print
#
def _print(print_str):
	if print2terminal == True: print print_str
	if write2file == True: output_file.write(print_str + "\n")
def _println():
	if print2terminal == True: print
	if write2file == True: output_file.write("\n")

###################################### HANDLE ARGS #######################################
argv = sys.argv
if len(argv) >= 2:
	arg1 = list(argv[1].replace('-','').lower())
	if 'p' in arg1: print "Printing to terminal:\tON"; print2terminal = True
	if 'r' in arg1: print "Search recursively:\tON"; srec = True
	if 'a' in arg1: print "Search all roots:\tON"; sall = True
	if 'm' in arg1: print "Only meta:\t\tON"; onlyMeta = True
	if len(argv) == 3:
		try:
			output_file = open(str(argv[2]), "w")
			write2file = True
			print "Output file set to:\t" + str(argv[2])
		except: print "Invalud Output File: " + str(argv[2])
	if len(argv) > 3:
		print "correct usage: data_inv [-par] [outputfile.txt]"
		sys.exit(1)

##################################### PROC_FILES ########################################

# proc_files(files): Processes all the files in input 'files'
#
# files: a list of files to process
#
def proc_files(rid, rootdir, root, files, meta):
	
	## VARIABLES & MISC
	X = 0; Y = 1
	nfiles = 0
	
	## Extract metadata from meta.txt file (check the current and root directory)
	if meta['platform'] == None: platform = "null" 
	else: platform = "'" + meta['platform'] + "'"
	
	if meta['product'] == None: product = "null"
	else: product = "'" + meta['product'] + "'"
	
	if meta['user'] == None: uname = "null"
	else: uname = "'" + meta['user'] + "'"
	
	if meta['src'] == None: source = "null"
	else: source = "'" + meta['src'] + "'"
	
	if meta['src_url'] == None: srcurl = "null"
	else: srcurl = "'" + meta['src_url'] + "'"
	
	if meta['pub_link'] == None: plink = "null"
	else: plink = "'" + meta['pub_link'] + "'"
	
	if meta['rest'] == None: rest = "null"
	else: rest = "'" + meta['rest'] + "'"
	
	if meta['scode'] == None: scode = "null"
	else: scode = "'" + meta['scode'] + "'"
	
	if meta['level'] == None: plevel = "null"
	else: plevel = "'" + meta['level'] + "'"
	
	if meta['res_dd'] == '': res_dd = "null"
	else: res_dd = "" + meta['res_dd'] + ""
	
	if meta['res_m'] == '': res_m = "null"
	else: res_m = "" + meta['res_m'] + ""
	
	if meta['notes'] == None: notes = "null"
	else: notes = "'" + meta['notes'] + "'"
	
	# Loop through the files in the current directory
	for f in files:
		# Loop through the supported file types
		for filetype in FILE_TYPES:
			# If the file is of a supported type, process it
			if f.endswith(filetype):
				
				try:
					
					## Check for file in noinv table
					dbResult = db.query("SELECT id FROM noinv WHERE fname = '" + f + "' AND path='" + root + "';").getresult()
					
					if len(dbResult) > 0:
						# This file should not be inventoried
						_print("\tfile on noinv list:\t" + root + f); continue
					
					
					## CHECK FOR DUPLICATES
					
					# Get file that has the same name and directory as the current file (if one exists)
					dupResult = db.query("SELECT id FROM inventory WHERE fname = '" + f + "' AND path='" + root + "';").getresult()
					
					# If the file exists in the database
					if len(dupResult) > 0:
						# There is a duplicate, skip this file
						_print("\tduplicate exists:\t" + root + f);nfiles += 1; continue
					
					## OPEN FILE
					try: dataset = gdal.Open(f,GA_ReadOnly)
					except: _print("could not open file: " + root + f); continue
					
					fname = f
					path = root
					
					## GET SRID
					srid = 4326
					try:
						prj = dataset.GetProjectionRef()
						srs = osr.SpatialReference(prj.title())
						srid = srs.GetAuthorityCode('GEOGCS')
					except: _print("\tno srid found")
					if srid == None: srid = 4326
					
					## GET FILE TYPE
					try: ftype = dataset.GetDriver().LongName
					except: ftype = ""
					
					## GET RESOLUTION
					try:
						resx = abs(dataset.GetGeoTransform()[1])
						resy = abs(dataset.GetGeoTransform()[5])
						#print "resolution: " + str(resx) + " x " + str(resy)
					except: resx = 0; resy = 0
					
					# cc (corner coordinates) is a list with format: 
					# [UL lon, UL lat, LL lon, LL lat, UR lon, UR lat, LR lon, LR lat]
					error, cc = geoutil.getCornerCoordinates(f)
					
					#if error == -1: _print("error (geoutil): No corner coordinates found."); continue
					#elif error == -2: _print("error (geoutil): Extent out of bounds") ; continue
					#elif error == -3: _print("error (geoutil): Max Extent used. Not recorded") #; continue
					
					# Convert coordinates (cc) to well known text (WKT) format
					wkt = geomops.geom2WKT(cc[TLO], cc[TLA], cc[TRO], cc[TRA], cc[BRO], cc[BRA], cc[BLO], cc[BLA], 'Descending')
					
					
					## UPDATAE INVENTORY TABLE
					db.query("INSERT INTO inventory (rid,path,fname,ftype,uname,platform,product,res_m,res_dd,source,srcurl,plink,rest,scode,plevel,notes,srid,wkt,geom) VALUES ('" + 
										str(rid) + "','" +
										str(path) + "','" +
										str(fname) + "','" +
										str(ftype) + "'," +
										uname + "," +
										platform + "," +
										product + "," +
										res_m + "," +
										res_dd + "," +
										source + "," +
										srcurl + "," +
										plink + "," +
										rest + "," +
										scode + "," +
										plevel + "," +
										notes + ",'" +
										str(srid) + "'," +
										"st_astext(st_geomfromtext(" + wkt + "))," +
										"st_geomfromtext(" + wkt + "));")
					
					
					_print("\tprocessed: \t\t" + root + f)
					nfiles += 1
				except Exception, dbmsg: _print("\tunable to process metadata for " + root + f + " " + str(dbmsg)+"\n"); continue
	return nfiles
####################################### FIND FILES #######################################
## Main Program:

# Connect to database
print "connecting to DB..."
try: db = pg.connect(dbname=DB_NAME, host=DB_HOST, user=DB_USER); print "connected."
except Exception, dbmsg: _print("Error connecting to DB: " + str(dbmsg)); sys.exit(1)

# var to keep track of sub directories in current root directory
tsubdir = 0; tfiles = 0
time = ""
	
try:
	if srec == True:
		# Find files in directory RECURSIVELY and process
		for root, dirs, files in os.walk("/mnt/"):
			# Make sure that the directory name ends in a back slash
			if not root.endswith('/'):
				root = root + "/"
			_print("\n\n" + root)
			os.chdir(root)
         
      	meta = geoutil.getMetaInfo(root)
      	# If the meta.txt file is not found in the directory:
      	if meta != -1:
            nfiles = proc_files(rid, rootdir, root, files, meta)
            if nfiles > 0:
			time = " last_update='now'"
			# check if directory exists in db
			dupRoot = db.query("SELECT id FROM roots WHERE path = '" + str(root) + "';").getresult()
				
			# If the file exists in the database
			if len(dupRoot) > 0:
			   db.query("UPDATE roots SET sdir='" + str(tsubdir) + "', nfiles='" + str(tfiles) + "' " + time + " WHERE id='" + str(dupRoot[0]) + "';")
			else:
			   db.query("INSERT INTO roots (path, nfiles, last_update) VALUES ('" + str(root) + "', '" + str(tfiles) + "', " + time + ");")
   
	else:
		# Find files ONLY in current directory
		files = os.listdir(rootdir)
		os.chdir(rootdir)
		_print("\n\n" + rootdir)
		nfiles = proc_files(rid, rootdir, rootdir, files)
		if nfiles < 0: time = ""
		else: tfiles += nfiles; time = ", last_update='now'"
			
except Exception, dbmsg: print "\n\ndirectory not found: " + str(dbmsg)
	
if nfiles > 0: time = ", last_update='now'"
else: time = ""
	
db.query("UPDATE roots SET sdir='" + str(tsubdir) + "', nfiles='" + str(tfiles) + "' " + time + " WHERE id='" + str(rid) + "';")
time = ""
	
	
