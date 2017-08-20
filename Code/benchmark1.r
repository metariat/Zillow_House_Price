train = merge(transactions, properties, by = "id.parcel", all.x = T)

train = train[, -c("material", "architechtural.style", "region.city",
                                     "region.neighbor", "region.zip", "story", "date", "id.parcel")]
train[is.na(train)] = -1

mod = randomForest(logerror ~ ., data= train, ntree= 10)


