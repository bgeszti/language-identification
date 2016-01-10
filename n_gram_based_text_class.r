library(plyr)
library(textcat)
library(tm)
library(tau)
library(data.table)
library(hash)

#Import documents  
docs <- Corpus(DirSource(directory = ".",encoding = "UTF-8",mode = "text"),
readerControl = list(reader = readPlain))

#Remove sentence ids, keep only the sentences
f <- content_transformer(function(x) gsub(".*\t","", x))
docs <- tm_map(docs, f)

#Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))

#Remove numbers
docs <- tm_map(docs, removeNumbers)

#Remove punctuations
docs <- tm_map(docs, removePunctuation, preserve_intra_word_dashes = TRUE)

#Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
docs = tm_map(docs, PlainTextDocument)

#Add language meta data to each of the documents in the Corpus
languages=list.files(path = ".")
languages=gsub("_.*","",languages)
	for (i in 1:length(docs)) {
		DublinCore(docs[[i]], "language") <- languages[i]
	}

#Create language profiles from the training set using the 300 most frequent n-grams in the languages
trainsize=1000;
h=hash()
	for (i in 1:length(docs)){
		train_text=docs[[i]]$content[1:trainsize]
		lang=docs[[i]]$meta$language
		.set(h, lang, train_text)
	}
train_list=as.list.hash(h)
lang_profiles = textcat_profile_db(train_list,id=names(train_list),size=300)

#Create test sets with 100 sentences per language
testsize=500;
h=hash()
	for (i in 1:length(docs)){
		train_text=docs[[i]]$content[(trainsize+1):(trainsize+testsize)]
		lang=docs[[i]]$meta$language
		.set(h, lang, train_text)
	}
test_list=as.list.hash(h)

#Evaluate accuracy
h=c()
	for (i in 1:length(test_list)){
		for (j in 1:length(test_list[[i]])){
			#categorize sentence
			d = textcat_xdist(test_list[[i]][j], p=lang_profiles, method="CT")
			lang=colnames(d)[which(d == min(d), arr.ind = TRUE)[2]]
			orig_lang=names(test_list)[i]
			h=rbind(h,c(orig_lang,lang))
			#print the counter to check how long the process will take
			print(i)
		}
	}
colnames(h)=c("original","predicted")
res = split(h[,'predicted'], h[,'original']) 

acc=c()
	for (i in 1:length(res)){
		ac=(sum(res[[i]]==names(res[i])))/(length(res[[i]]))
		acc=rbind(acc,c(names(res[i]),ac))
	}

#Sorting the distances for languages so see the closest ones
res=t(apply(d,1,sort))

#Language correlation graph based on the language profiles
library(plyr)
library(reshape2)
library(corrplot)
library(igraph)
d = textcat_xdist(lang_profiles)
G <- graph.adjacency(d,weighted=TRUE);
MST=minimum.spanning.tree(G,algorithm="prim");
MST <- simplify(as.undirected(MST))
plot(MST,vertex.shape="circle",layout = layout.kamada.kawai,vertex.color="white",vertex.size=0,vertex.label.cex=0.9,vertex.label.font=2,vertex.label.color="black",asp=0, margin=0.1)


#n-gram creation for building language profiles (this is built in the textcat package)
	for (i in 1:length(docs)){
		r=textcnt(content(docs[[i]])[1:trainsize], method="ngram",n=5L, split = "[[:space:][:punct:][:digit:]]+",decreasing=TRUE,size=300)
		lang=meta(docs[[i]])$language		
		.set(h, lang, names(langprof ))
	}
profiles=as.list.hash(h)