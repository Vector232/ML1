sortObjectsByDist = function(xl, z){
    
    l = dim(xl)[1];
    n = dim(xl)[2]-1;
    
    distances = matrix(NA, l, 2);
    
    for (i in 1:l){
        distances[i] = sqrt(sum((xl[i, 1:n] - z)^2));
    }
    
    return (distances);
}

core_1 = function(r){
    
    if(r <= 1){
        return((3/4)*(1-r^2))
    }
    else
        return(0)
}

core_2 = function(r){
    
    if(r <= 1){
        return((15/16)*(1-r^2)^2)
    }
    else
        return(0)
}

core_3 = function(r){
    
    if(r <= 1){
        return((1-abs(r)))
    }
    else
        return(0)
}

core_4 = function(r)
{
    return (((2*pi)^(-1/2)) * exp(-1/2*r^2)) 
}


classif_PW = function(xl, h, orderedXl, ForSTOLP, core = core_4) {
    l = dim(xl)[1];
    n = dim(xl)[2];
    
    weights = table(xl[1:l, n]);
    weights[1:dim(weights)] = 0;
    
    for (i in 1:l) {
        weights[xl[i, n]] = weights[xl[i, n]] + core(orderedXl[i] / h);
    }
    
    if (max(weights) != 0)
        if(ForSTOLP == TRUE)
           return (max(weights))
        else
           return (which.max(weights))
    else
        return (0)
}

PW = function(xl, h, z, ForSTOLP){
    return(classif_PW(xl, h, sortObjectsByDist(xl, z), ForSTOLP));
}

risk = function(xl, classes, point, class){
    
    thisclasses = xl[which(classes == class),]
    otherclasses = xl[which(classes != class),]
   
    thisrisk = PW(thisclasses, 0.1, point[1:2],TRUE)
    otherrisk = PW(otherclasses, 0.1, point[1:2],TRUE)
        
    return(thisrisk-otherrisk)
}

stolp = function(xl, classes,errors) {
    n = length(classes)
    #считаем все риски(отступы)
    risk = array(0, n)
    for (i in 1:n){
        risk[i] = risk(xl, classes, xl[i,], classes[i])
        print(i)
        print(risk[i])
    }
    
    
    
    
    #plot(1:n, sort(risk), col="black", bg="blue",pch=20,main = "Риски",ylab = "Риск ", xlab = "Данные")
    #lines(1:n, sort(risk), lwd = 2, col = "black")
    
    #удаляем объекты с рисками меньше нуля, как ошибочные и шумовые
    badpoints = which(risk < 0)
    pointsWE=xl[-badpoints,]
    classes = classes[-badpoints]
    n = n - length(badpoints)
    etalone = data.frame()
    #объекты с максимальным риском являются эталонным
    for (class in unique(classes)) {
        ind = which(class == classes )
        risk = sapply(ind, function(i) risk(pointsWE, classes, pointsWE[i,], class))
        maxrisk = ind[which.max(risk)]
        etalone=rbind(etalone, pointsWE[maxrisk,])
        pointsWE=pointsWE[-maxrisk,]
        classes=classes[-maxrisk]
        n = n - 1
    }
    # пересчитали величины риска для всех объектов и среди объектов каждого класса, распознанных неправильно, 
    # выбрать объекты с максимальной величиной риска и добавить их к эталонам
    names(etalone) = names(xl)
    while(n!=length(etalone)){
        count=0
        risk = c()
        index = c()
        for(i in 1:n)
        {
            m = risk(etalone, etalone[,3], pointsWE[i,], classes[i])
            if(m<=0){
                count=count+1;
                risk = c(risk, m)
                index = c(index, i)
            }
        }
        if( count < errors ) {
            print("errors")
            print(count)
            break
        }
        
        
        minrisk = index[which.min(risk)]
        etalone = rbind(etalone, pointsWE[minrisk,])
        pointsWE = pointsWE[-minrisk,]
        classes = classes[-minrisk]
        n = n - 1
        
    }
    print(etalone)
}
xl = iris[,3:5]
classes = xl[,3]
#print(classes[3])
#print(xl[which(classes == xl[1,3]),])
stolp(xl, classes, 7)
