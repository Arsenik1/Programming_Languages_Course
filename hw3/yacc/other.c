#include "other.h"

//global head;
node_t *head= NULL;
node_t *temp = NULL;
void addNewVariable(char name[30], fraction_t data){
    node_t *iter;
    iter=head;
    if (head==NULL) {
        head=(node_t *)malloc(sizeof(node_t));
        head->data.pay = data.pay;
		head->data.payda = data.payda;
        strcpy(head->name, name);
        head->next=NULL;
    }
    else{
        while(iter->next!=NULL){
            iter=iter->next;
        }
        node_t *temp1=(node_t *)malloc(sizeof(node_t));
        temp1->data.pay = data.pay;
		temp1->data.payda = data.payda;
        strcpy(temp1->name, name);
        temp1->next=iter->next;
        iter->next =temp1;
    }
}

fraction_t getDataOfVariable(char* name){
    node_t *iter;
    iter=head;
    while(iter != NULL){
        if(strcmp(iter->name, name) == 0){
            return iter->data;
		}
        iter=iter->next;
    }
	fraction_t fr;
	fr.pay = -999;
	fr.payda = 1;
    return fr;// if not found return -999
}

/*append operation on linked list*/
fraction_t* appendElementToList(fraction_t *list, fraction_t element){
    node_t *iter;
    iter=temp;
    if (temp==NULL) {
        temp=(node_t *)malloc(sizeof(node_t));
        temp->data=element;
        temp->next=NULL;
        list = (fraction_t *)malloc(sizeof(fraction_t)*2);
        list[0] = element;
	}
    else{
        int size = 0;
        while(iter->next!=NULL){
            size++;
            iter=iter->next;
        }
        size++;
        node_t *temp1=(node_t *)malloc(sizeof(node_t));
        temp1->data=element;
        temp1->next=iter->next;
        iter->next =temp1;
        list = (fraction_t*)(malloc(sizeof(fraction_t)*(size+2)));
        int i = 0;
        iter=temp;
        while(iter != NULL){
            list[i] = iter->data;
            i++;
            iter=iter->next;
		}
        list[i].pay = -999;
    }
    return list;
}

/*concat operation on linked list*/
fraction_t* concatTwoList(fraction_t *list1, fraction_t *list2){
    return list2;
}

fraction_t convertToPayPayda(char str[30])
{
    fraction_t result;
    result.pay = atoi(strtok(str, "b"));
    result.payda = atoi(strtok(NULL, "b"));
    return result;
}

char* convertToString(fraction_t fraction) {
    // Write the formatted string into result
    char* result = (char*)malloc(sizeof(char)*30);
    sprintf(result, "%db%d", fraction.pay, fraction.payda);
    return result;
}

fraction_t simplifyFraction(fraction_t fraction){
    int i = 2;
    while(i <= fraction.pay && i <= fraction.payda){
        if(fraction.pay % i == 0 && fraction.payda % i == 0){
            fraction.pay /= i;
            fraction.payda /= i;
        }
        else{
            i++;
        }
    }
    return fraction;
}

fraction_t addTwoFraction(fraction_t fraction1, fraction_t fraction2){
    fraction_t result;
	if(fraction1.pay != -999 && fraction2.pay != -999 && fraction1.payda != 0 && fraction2.payda != 0){
    result.payda = fraction1.payda * fraction2.payda;
    result.pay = fraction1.pay * fraction2.payda + fraction2.pay * fraction1.payda;

    result = simplifyFraction(result);
    return result;
	}
	else{
		result.pay = -999;
		result.payda = 1;
		return result;
	}
}

fraction_t subtractTwoFraction(fraction_t fraction1, fraction_t fraction2){
	fraction_t result;
	if(fraction1.pay != -999 && fraction2.pay != -999 && fraction1.payda != 0 && fraction2.payda != 0){
	result.payda = fraction1.payda * fraction2.payda;
	result.pay = fraction1.pay * fraction2.payda - fraction2.pay * fraction1.payda;

	result = simplifyFraction(result);
	return result;
	}
	else{
		result.pay = -999;
		result.payda = 1;
		return result;
	}
}

fraction_t multiplyTwoFraction(fraction_t fraction1, fraction_t fraction2){
	fraction_t result;
	if(fraction1.pay != -999 && fraction2.pay != -999 && fraction1.payda != 0 && fraction2.payda != 0){
	result.payda = fraction1.payda * fraction2.payda;
	result.pay = fraction1.pay * fraction2.pay;

	result = simplifyFraction(result);
	return result;
	}
	else{
		result.pay = -999;
		result.payda = 1;
		return result;
	}
}

fraction_t divideTwoFraction(fraction_t fraction1, fraction_t fraction2){
	fraction_t result;
	if(fraction1.pay != -999 && fraction2.pay != -999 && fraction1.payda != 0 && fraction2.payda != 0){
	result.payda = fraction1.payda * fraction2.pay;
	result.pay = fraction1.pay * fraction2.payda;

	result = simplifyFraction(result);
	return result;
	}
	else{
		result.pay = -999;
		result.payda = 1;
		return result;
	}
}

int compareTwoFraction(fraction_t fraction1, fraction_t fraction2){
    fraction_t result = subtractTwoFraction(fraction1, fraction2);
    if(result.pay == 0){
        return 0;
    }
    else if(result.pay > 0){
        return 1;
    }
    else{
        return -1;
    }
}

void printFraction(fraction_t fraction){
	// printf("%d/%d\n", fraction.pay, fraction.payda);
	fprintf(fileOutput, "%d/%d\n", fraction.pay, fraction.payda);
}