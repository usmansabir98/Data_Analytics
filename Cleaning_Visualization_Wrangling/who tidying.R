data(who)
View(who)

who %>% gather(new_sp_m014:newrel_f65, key='key', value= 'value') %>% group_by(key) %>% count() %>% View

#HW
# first separate newrel into new_rel
# 1 col- new/old
# 2 col - type of tb (sp/ep/sn/rel)
# 3 col - gender (m/f)
# 4 col - age(15-24 etc)
# 5 col - val (no of cases) values that exist in cells

# remove 2 out of 3 col for iso naming

names(who)
newrel_vect <- grep('newrel', names(who), value=TRUE)
indices <- grep('newrel', names(who))

for(i in indices){
  names(who)[i] <- paste(substr(x=names(who)[i], 1, 3), substr(x=names(who)[i], 4, nchar(names(who)[i])), sep="_")
}

who %>% gather(key = key, value = count, 5:60) %>% View
who %>% gather(key = key, value = count, 5:60) -> who1

who1 %>% separate(key, into=c("new/old", "type", "genderAge")) %>% View
who1 %>% separate(key, into=c("new/old", "type", "genderAge")) -> who2

View(who2)
who2$genderAge <- paste(substr(x=who2$genderAge, 1, 1), substr(x=who2$genderAge, 2, 5), sep="_")

who2 %>% separate(genderAge, into = c("gender", "age")) %>% View
who2 %>% separate(genderAge, into = c("gender", "age")) -> who3

who3$count[is.na(who2$count)] <-0

View(who3)
class(who3$age)

age <- who3$age

newAge <- sapply(age, function(a){
  if(nchar(a)==2){
    paste(">", a, sep="")
  }
  else if(nchar(a)==3){
    paste(substr(x=a, 1, 1), substr(x=a, 2, 3), sep="-")
  }
  else{
    paste(substr(x=a, 1, 2), substr(x=a, 3, 4), sep="-")
  }
  
})

View(newAge)

age <- newAge
who4 <- who3
who4$age <- age
View(who4)

# changing gender values m to male and f to female
gender <- who4$gender
class(gender)

male_indices <- grep("m", gender, ignore.case = TRUE)
gender[male_indices] <- "male"

female_indices <- grep("f", gender, ignore.case = TRUE)
gender[female_indices] <- "female"

View(gender)

who4$gender <- gender



