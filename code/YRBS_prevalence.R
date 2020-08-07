# Coder: Anh Tran
# Objective: Calculate prevalence


# Demographic characteristics
setDT(yrbs_data)
describe(yrbs_data[,.(year,sex,grade,race4,sexid2)]) # Hmisc package

# suicide risk
yrbs_data %>%
  count(qnsuicide)


# When analyzing survey data, the sampling design information is packaged with the data
# into a ‘survey design object’, and this object is part of the input to analyses.
yrbsdes <- svydesign(id=~PSU, weight=~weight, strata=~stratum,
                     data=yrbs_data, nest=TRUE)


# Demographic characteristics of HS students by level of suicide risk
## Sex
svytable(~sex+qnsuicide, yrbsdes) %>% prop.table(2) # percent
svyciprop(~I(sex=="Female"), subset(yrbsdes,qnsuicide==0), method="lo",  level = 0.95)
svyciprop(~I(sex=="Female"), subset(yrbsdes,qnsuicide==1), method="lo",  level = 0.95)
svyciprop(~I(sex=="Male"), subset(yrbsdes,qnsuicide==0), method="lo",  level = 0.95)
svyciprop(~I(sex=="Male"), subset(yrbsdes,qnsuicide==1), method="lo",  level = 0.95)
svychisq(~sex+qnsuicide, yrbsdes) 

## Race
svytable(~race4+qnsuicide, yrbsdes) %>% prop.table(2)
svyciprop(~I(race4=="01.White"), subset(yrbsdes,qnsuicide==0), method="lo",  level = 0.95)
svyciprop(~I(race4=="01.White"), subset(yrbsdes,qnsuicide==1), method="lo",  level = 0.95)
svyciprop(~I(race4=="02.Black/African American"), subset(yrbsdes,qnsuicide==0), method="lo",  level = 0.95)
svyciprop(~I(race4=="02.Black/African American"), subset(yrbsdes,qnsuicide==1), method="lo",  level = 0.95)
svyciprop(~I(race4=="03.Latino/Hispanic"), subset(yrbsdes,qnsuicide==0), method="lo",  level = 0.95)
svyciprop(~I(race4=="03.Latino/Hispanic"), subset(yrbsdes,qnsuicide==1), method="lo",  level = 0.95)
svyciprop(~I(race4=="04.All Other Races"), subset(yrbsdes,qnsuicide==0), method="lo",  level = 0.95)
svyciprop(~I(race4=="04.All Other Races"), subset(yrbsdes,qnsuicide==1), method="lo",  level = 0.95)
svychisq(~race4+qnsuicide, yrbsdes) 

## Grade
svytable(~grade+qnsuicide, yrbsdes) %>% prop.table(2)
svyciprop(~I(grade=="9th grade"), subset(yrbsdes,qnsuicide==0), method="lo",  level = 0.95)
svyciprop(~I(grade=="9th grade"), subset(yrbsdes,qnsuicide==1), method="lo",  level = 0.95)
svyciprop(~I(grade=="10th grade"), subset(yrbsdes,qnsuicide==0), method="lo",  level = 0.95)
svyciprop(~I(grade=="10th grade"), subset(yrbsdes,qnsuicide==1), method="lo",  level = 0.95)
svyciprop(~I(grade=="11th grade"), subset(yrbsdes,qnsuicide==0), method="lo",  level = 0.95)
svyciprop(~I(grade=="11th grade"), subset(yrbsdes,qnsuicide==1), method="lo",  level = 0.95)
svyciprop(~I(grade=="12th grade"), subset(yrbsdes,qnsuicide==0), method="lo",  level = 0.95)
svyciprop(~I(grade=="12th grade"), subset(yrbsdes,qnsuicide==1), method="lo",  level = 0.95)
svychisq(~qnsuicide+grade, yrbsdes) 

## Sexual identity
svytable(~sexid2+qnsuicide, yrbsdes) %>% prop.table(2)
svyciprop(~I(sexid2=="01.Heterosexual"), subset(yrbsdes,qnsuicide==0), method="lo",  level = 0.95)
svyciprop(~I(sexid2=="01.Heterosexual"), subset(yrbsdes,qnsuicide==1), method="lo",  level = 0.95)
svyciprop(~I(sexid2=="02.Sexual Minority"), subset(yrbsdes,qnsuicide==0), method="lo",  level = 0.95)
svyciprop(~I(sexid2=="02.Sexual Minority"), subset(yrbsdes,qnsuicide==1), method="lo",  level = 0.95)
svyciprop(~I(sexid2=="03.Unsure"), subset(yrbsdes,qnsuicide==0), method="lo",  level = 0.95)
svyciprop(~I(sexid2=="03.Unsure"), subset(yrbsdes,qnsuicide==1), method="lo",  level = 0.95)
svychisq(~sexid2+qnsuicide, yrbsdes)


# ANOVA for sexual identity
# Compute the analysis of variance
res.aov <- aov(qnsuicide ~ sexid2, data = yrbsdes$variables)
# Summary of the analysis
summary(res.aov)
TukeyHSD(res.aov)

#################################################################
# Prevalence of health-related behaviros by level of suicide risk

# Community-related violence

svytable(~qn17+qnsuicide, yrbsdes) %>% prop.table(2)
svyciprop(~I(qn17==1), subset(yrbsdes,qnsuicide==0), method="lo",  level = 0.95)
svyciprop(~I(qn17==1), subset(yrbsdes,qnsuicide==1), method="lo",  level = 0.95)
svychisq(~qnsuicide+qn17, yrbsdes) 

svytable(~qn24+qnsuicide, yrbsdes) %>% prop.table(2)
svyciprop(~I(qn24==1), subset(yrbsdes,qnsuicide==0), method="lo",  level = 0.95)
svyciprop(~I(qn24==1), subset(yrbsdes,qnsuicide==1), method="lo",  level = 0.95)
svychisq(~qnsuicide+qn24, yrbsdes) 

svytable(~qn19+qnsuicide, yrbsdes) %>% prop.table(2)
svyciprop(~I(qn19==1), subset(yrbsdes,qnsuicide==0), method="lo",  level = 0.95)
svyciprop(~I(qn19==1), subset(yrbsdes,qnsuicide==1), method="lo",  level = 0.95)
svychisq(~qnsuicide+qn19, yrbsdes) 

# School-related violence

svytable(~qn13+qnsuicide, yrbsdes) %>% prop.table(2)
svyciprop(~I(qn13==1), subset(yrbsdes,qnsuicide==0), method="lo",  level = 0.95)
svyciprop(~I(qn13==1), subset(yrbsdes,qnsuicide==1), method="lo",  level = 0.95)
svychisq(~qnsuicide+qn13, yrbsdes) 

svytable(~qn18+qnsuicide, yrbsdes) %>% prop.table(2)
svyciprop(~I(qn18==1), subset(yrbsdes,qnsuicide==0), method="lo",  level = 0.95)
svyciprop(~I(qn18==1), subset(yrbsdes,qnsuicide==1), method="lo",  level = 0.95)
svychisq(~qnsuicide+qn18, yrbsdes) 

svytable(~qn23+qnsuicide, yrbsdes) %>% prop.table(2)
svyciprop(~I(qn23==1), subset(yrbsdes,qnsuicide==0), method="lo",  level = 0.95)
svyciprop(~I(qn23==1), subset(yrbsdes,qnsuicide==1), method="lo",  level = 0.95)
svychisq(~qnsuicide+qn23, yrbsdes) 

svytable(~qn15+qnsuicide, yrbsdes) %>% prop.table(2)
svyciprop(~I(qn15==1), subset(yrbsdes,qnsuicide==0), method="lo",  level = 0.95)
svyciprop(~I(qn15==1), subset(yrbsdes,qnsuicide==1), method="lo",  level = 0.95)
svychisq(~qnsuicide+qn15, yrbsdes) 

svytable(~qn16+qnsuicide, yrbsdes) %>% prop.table(2)
svyciprop(~I(qn16==1), subset(yrbsdes,qnsuicide==0), method="lo",  level = 0.95)
svyciprop(~I(qn16==1), subset(yrbsdes,qnsuicide==1), method="lo",  level = 0.95)
svychisq(~qnsuicide+qn16, yrbsdes) 

# Mental Health issue

svytable(~qn25+qnsuicide, yrbsdes) %>% prop.table(2)
svyciprop(~I(qn25==1), subset(yrbsdes,qnsuicide==0), method="lo",  level = 0.95)
svyciprop(~I(qn25==1), subset(yrbsdes,qnsuicide==1), method="lo",  level = 0.95)
svychisq(~qnsuicide+qn25, yrbsdes) 

# Substance use

svytable(~qn42+qnsuicide, yrbsdes) %>% prop.table(2)
svyciprop(~I(qn42==1), subset(yrbsdes,qnsuicide==0), method="lo",  level = 0.95)
svyciprop(~I(qn42==1), subset(yrbsdes,qnsuicide==1), method="lo",  level = 0.95)
svychisq(~qnsuicide+qn42, yrbsdes) 

svytable(~qn48+qnsuicide, yrbsdes) %>% prop.table(2)
svyciprop(~I(qn48==1), subset(yrbsdes,qnsuicide==0), method="lo",  level = 0.95)
svyciprop(~I(qn48==1), subset(yrbsdes,qnsuicide==1), method="lo",  level = 0.95)
svychisq(~qnsuicide+qn48, yrbsdes) 

svytable(~qn52+qnsuicide, yrbsdes) %>% prop.table(2)
svyciprop(~I(qn52==1), subset(yrbsdes,qnsuicide==0), method="lo",  level = 0.95)
svyciprop(~I(qn52==1), subset(yrbsdes,qnsuicide==1), method="lo",  level = 0.95)
svychisq(~qnsuicide+qn52, yrbsdes) 

svytable(~qn57+qnsuicide, yrbsdes) %>% prop.table(2)
svyciprop(~I(qn57==1), subset(yrbsdes,qnsuicide==0), method="lo",  level = 0.95)
svyciprop(~I(qn57==1), subset(yrbsdes,qnsuicide==1), method="lo",  level = 0.95)
svychisq(~qnsuicide+qn57, yrbsdes) 

# Sexual health

svytable(~qn60+qnsuicide, yrbsdes) %>% prop.table(2)
svyciprop(~I(qn60==1), subset(yrbsdes,qnsuicide==0), method="lo",  level = 0.95)
svyciprop(~I(qn60==1), subset(yrbsdes,qnsuicide==1), method="lo",  level = 0.95)
svychisq(~qnsuicide+qn60, yrbsdes) 

svytable(~qn62+qnsuicide, yrbsdes) %>% prop.table(2)
svyciprop(~I(qn62==1), subset(yrbsdes,qnsuicide==0), method="lo",  level = 0.95)
svyciprop(~I(qn62==1), subset(yrbsdes,qnsuicide==1), method="lo",  level = 0.95)
svychisq(~qnsuicide+qn62, yrbsdes) 

# Weight-related issue

svytable(~qnobese+qnsuicide, yrbsdes) %>% prop.table(2)
svyciprop(~I(qnobese==1), subset(yrbsdes,qnsuicide==0), method="lo",  level = 0.95)
svyciprop(~I(qnobese==1), subset(yrbsdes,qnsuicide==1), method="lo",  level = 0.95)
svychisq(~qnsuicide+qnobese, yrbsdes) 

svytable(~qn68+qnsuicide, yrbsdes) %>% prop.table(2)
svyciprop(~I(qn68==1), subset(yrbsdes,qnsuicide==0), method="lo",  level = 0.95)
svyciprop(~I(qn68==1), subset(yrbsdes,qnsuicide==1), method="lo",  level = 0.95)
svychisq(~qnsuicide+qn68, yrbsdes) 

# Sedentary activities

svytable(~qn80+qnsuicide, yrbsdes) %>% prop.table(2)
svyciprop(~I(qn80==1), subset(yrbsdes,qnsuicide==0), method="lo",  level = 0.95)
svyciprop(~I(qn80==1), subset(yrbsdes,qnsuicide==1), method="lo",  level = 0.95)
svychisq(~qnsuicide+qn80, yrbsdes) 

svytable(~qn81+qnsuicide, yrbsdes) %>% prop.table(2)
svyciprop(~I(qn81==1), subset(yrbsdes,qnsuicide==0), method="lo",  level = 0.95)
svyciprop(~I(qn81==1), subset(yrbsdes,qnsuicide==1), method="lo",  level = 0.95)
svychisq(~qnsuicide+qn81, yrbsdes)

svytable(~qn79+qnsuicide, yrbsdes) %>% prop.table(2)
svyciprop(~I(qn79==1), subset(yrbsdes,qnsuicide==0), method="lo",  level = 0.95)
svyciprop(~I(qn79==1), subset(yrbsdes,qnsuicide==1), method="lo",  level = 0.95)
svychisq(~qnsuicide+qn79, yrbsdes)

svytable(~qn83+qnsuicide, yrbsdes) %>% prop.table(2)
svyciprop(~I(qn83==1), subset(yrbsdes,qnsuicide==0), method="lo",  level = 0.95)
svyciprop(~I(qn83==1), subset(yrbsdes,qnsuicide==1), method="lo",  level = 0.95)
svychisq(~qnsuicide+qn83, yrbsdes)

svytable(~qn88+qnsuicide, yrbsdes) %>% prop.table(2)
svyciprop(~I(qn88==1), subset(yrbsdes,qnsuicide==0), method="lo",  level = 0.95)
svyciprop(~I(qn88==1), subset(yrbsdes,qnsuicide==1), method="lo",  level = 0.95)
svychisq(~qnsuicide+qn88, yrbsdes)

# Low grade

svytable(~qn89+qnsuicide, yrbsdes) %>% prop.table(2)
svyciprop(~I(qn89==1), subset(yrbsdes,qnsuicide==0), method="lo",  level = 0.95)
svyciprop(~I(qn89==1), subset(yrbsdes,qnsuicide==1), method="lo",  level = 0.95)
svychisq(~qnsuicide+qn89, yrbsdes)


