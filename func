# 1 - Model fit summary.
fit.summary<- function(fit)
{
    return(fitMeasures(fit, c("chisq", "df", "pvalue", "rmsea", "srmr", "CFI", "tli", "aic", "bic")))
}


# 2 - Modification indices sum.
mi.sum<- function(mi, mod.list) 
{
    # l1: number of factor
    l1<- length(mod.list)
    
    for (i in 1:l1)
    {
        f.item<- unlist(mod.list[i])
        # l2: number of item in one factor
        l2<- length(f.item)
        
        for (j in 2:l2)
        {
            item<- subset(mi, lhs== f.item[1] & rhs== f.item[j] & op== "=~")
            mi.sum<- sum(item$mi)
            
            if (j==2)
                mi.sum.vector<- mi.sum
            else
                mi.sum.vector<- c(mi.sum.vector, mi.sum)
        }
        if (i==1)
            mi.sum.list<- list(mi.sum.vector)
        else
            mi.sum.list[[i]]<- mi.sum.vector
    }
    return(mi.sum.list)
}
