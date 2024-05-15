# Classification of Bank Customer Account Activity Status

### Problem:

The main target of this data is to model the Customer Intelligence in the bank (name - unknown). It mainly focuses on analysing the client's behaviour, followed by estimating the client's behaviour, including external factors. Overall, this will allow the bank management team to predict whether the clients are active or inactive. If the client is non-active, it's an indication to take proactive actions to keep the client as a stable bank customer for the future, not allowing them to switch banks.

Customer Intelligence is described by 36 dimensional variables, of which, 6 are categorical and 30 are numerical. The customer's confidentiality was kept, so there is no association between the data and the customer, and the meaning behind each variable is unknown.

### Data:

- **client_train.txt**: the file consists of 12000 training patters, of which 6000 are marked as "Active" and 6000 as "Non-Active" clients. Each pattern as 37 values (36 are input attributes and the 37th is the output class), the first 6 represent values of nominal attributes, the next 30 represent values of real attributes and the last values represents the information regarding the class.

- **client_train_marked.txt**: the file consists of 12000 training patters, of which 6000 are marked as "Active" and 6000 as "Non-Active" clients. Each pattern has the same structure as the patterns in the file client_train.txt.

### Objective:

Binary classification problem - we want to classify wether a customer is "active" or "inactive".