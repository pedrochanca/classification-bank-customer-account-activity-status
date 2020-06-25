# Data-Mining-Project

### Definition of the Problem:

The main target of this data is to model the Customer Intelligence in the bank (name - unkown). It has a main focus on analysing the client's behaviour followed by an estimation of client's behaviour with the inclusion of external factors. Overall, this will allow the bank management team to predict if the clients are either active or non-active. If the client is non-active, it's an indication to take pro-active actions to keep the client as a stable customer of the bank for the future, not allowing him/her to switch banks.

The Customer Intelligence is described by 36 dimensional variables, of which 6 are nominal and 30 are real. The confidentiality of the customer was kept and therefore there is no association between the data and the customer as well as the meaning behind each variable is unkown.

### Description of the Datasets:

- **client_train.txt**: the file consists of 12000 training patters, of which 6000 are marked as "Active" and 6000 as "Non-Active" clients. Each pattern as 37 values (36 are input attributes and the 37th is the output class), the first 6 represent values of nominal attributes, the next 30 represent values of real attributes and the last values represents the information regarding the class.

- **client_train_marked.txt**: the file consists of 12000 training patters, of which 6000 are marked as "Active" and 6000 as "Non-Active" clients. Each pattern has the same structure as the patterns in the file client_train.txt.

### Goal:

To create classification tools to distinguish between "Active" and "Non-Active" classes - dichotomise classification.
