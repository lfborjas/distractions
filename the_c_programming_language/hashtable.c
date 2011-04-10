/*Based on K&R's book, second edition*/

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#define unless(cond) if(!cond)
#define HASHSIZE 101

struct nlist{
    char *name;
    char *def;
    struct nlist *next;
};

static struct nlist *hashtab[HASHSIZE];

unsigned hash(s)
char *s;
{
    unsigned hashval;

    for(hashval = 0; *s != '\0';s++)
        hashval = *s + 31  * hashval;

    return(hashval % HASHSIZE);
}

/*Find the string s in the hashtable*/
struct nlist *lookup(s)
char *s;
{
    struct nlist *np;

    for (np = hashtab[hash(s)]; np != NULL ;np = np->next) 
        unless (strcmp(s, np->name))
            return(np);

    return(NULL);
}

/*Insert/replace the string in the hashtable*/
struct nlist *install(name, def)
char *name, *def;
{
    struct nlist *np;
    unsigned hashval;

    if( (np = lookup(name)) == NULL  ){ //not found
        np = (struct nlist *) malloc(sizeof(*np));
        if(np == NULL  || (np->name = strdup(name)) == NULL) 
            return NULL;
        hashval = hash(name);
        np->next = hashtab[hashval];
        hashtab[hashval] = np;
    }else{
        free((void *) np->def);
    }
    if((np->def = strdup(def)) == NULL)
        return NULL;
    return np;
}

struct nlist *undef(name)
/*Remove a name and its definition from the table*/
char *name;
{
    struct nlist *np;
    if( (np = lookup(name)) != NULL ){
        free((void *) np->name);
        free((void *) np->def);
    }

    return np;
}


int main(){
    install("hello", "world");
    printf("%s", lookup("hello")->def);
    undef("hello");
    printf("%d", lookup("hello")==NULL);
    return 0;
}
