#[1] 0.05


### 1.3. Implement Benjamini-Hochberg method for the original data

# computing p-values
features_no = ncol(data0)-1 #4702
p = c()
for(f in 1:features_no){
  res = t.test(data0[,f]~Conference, data=data0, alternative="two.sided")
  p[f] = res$p.value
}
p #4702
order = order(p)
p = p[order]

# Define
alpha = 0.05
index = c()
for(j in 1:length(p)){
  if(p[j] < alpha*j/length(p)) index = c(index, j)
}
index

# reject all hypotheses H0j for which p[j] <= p[L]
L = max(index)
L
#[1] 39

names(data0)[order][1:39]
#[1] "papers"        "submission"    "position"      "published"     "important"     "call"          "conference"    "candidates"    "dates"         "paper"         "topics"        "limited"       "candidate"     "camera"        "ready"         "authors"       "phd"           "projects"      "org"           "chairs"        "due"           "original"      "notification"  "salary"        "record"        "skills"        "held"          "team"          "pages"         "workshop"      "committee"    
#[32] "proceedings"   "apply"  
