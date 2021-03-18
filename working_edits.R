
#RED WINE DATA EDITS

unique(is.na(winered))# no missing values
unique(is.na(winewhite)) #no missing values

summary(winered)

str(winered)

apply(winered, 2, function(x) length(unique(x)))

#all clearly continuous except quality, which has 6 possible values (on a scale of 10 bizarrely)

apply(winewhite, 2, function(x) length(unique(x)))

unique(winered$quality)

#Will make a dummy for the wine quality as "better" and "worse"

winered$quality2 <-ifelse((winered$quality>=4) & (winered$quality>=5),1,0)

winered$quality <- as.factor(winered$quality) #make original a factor

winered$quality2 <- as.factor(winered$quality2) #make dummy a factor

str(winered)

c(table(winered$quality2)) #number of categories, extremely unbalanced





#WHITE WINE DATA EDITS

summary(winewhite)

str(winewhite)

apply(winewhite, 2, function(x) length(unique(x)))

#all clearly continuous except quality, which has 7 possible values (on a scale of 10 bizzarely)

apply(winewhite, 2, function(x) length(unique(x)))

unique(winewhite$quality)

#Will make a dummy for the wine quality as "better" and "worse"

winewhite$quality2 <-ifelse((winewhite$quality>=4) & (winewhite$quality>=5),1,0)

winewhite$quality <- as.factor(winewhite$quality) #make original a factor

winewhite$quality2 <- as.factor(winewhite$quality2) #make dummy a factor

count(winewhite2$quality2)

c(table(winered$quality2)) #number of categories, extremely unbalanced



# Regressions

winewhitef = subset(winewhite, select = -c(quality2) )
winewhite2 = subset(winewhite, select = -c(quality) )


summary(vglm(quality~., data=winewhitef, family = cumulative(link = "logit", parallel = FALSE, reverse = TRUE)))

logregwinewhite <- glm(quality2~., data=winewhite2, family = binomial())
summary(logregwinewhite)

winewhite2 <- as.data.frame(winewhite2)

corrwinewhite <- cor(winewhite2)
corrplot(logregwinewhite)

winewhite$quality <- as.numeric(winewhite$quality) #make original a factor

#BUTTON HEAD

#myHeader <- div(id="advanced",
#               useShinyjs(),
#              downloadButton("report", "Generate report"),

# 


# header = myHeader,

# SORTING ON QUALITY

#selectInput("select", label = h3("Plot by Quality"), 
#            choices = list_choices,
#            selected = 1),

# col_scale <- scale_colour_discrete(limits = list_choices)

# attempted parameters

xsummary: xsummary
ysummary: ysummary
xhead: xhead
yhead: yhead


xsummary = isolate(xsummary)
ysummary = isolate(ysummary)
xhead = isolate(xhead)
yhead = isolate(yhead)