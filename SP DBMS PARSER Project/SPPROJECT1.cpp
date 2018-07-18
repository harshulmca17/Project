#include<iostream>
#include<cstdio>
#include<unistd.h>
#include<dos.h>
#include<sstream>
#include<cstring>
using namespace std;
void update(string sql);
void select(string sql);
void delet(string sql);
int main()
{
	string sql;
	string s="select",u="update",c="delete";
	
	int ch;
	cout<<"\nEnter the SQL Query : \n";
	getline(cin,sql);
	istringstream iss(sql);
    string *word;
    
    //while(iss >> word) {
	//	cout<<word<<endl;
        /* do stuff with word */
    //}
    
    int n=sql.size();
    word = new string [n];
    for (int i=0;iss!='\0';i++)
    {
    	iss >> word[i];
			
	}
	if (sql.find(s) != string::npos)
	{
		ch=1;	
	}
	else if(sql.find(u) != string::npos)
	{
		ch=2;
	}
	else if(sql.find(c) != string::npos)
	{
		ch=3;
	}
	else 
	{
		ch=4;
	}
	switch (ch)
	{
		case 1: select(sql);
				break;
		case 2: update(sql);
				break;
		case 3: delet(sql);
				break;
		case 4: cout<<"\nQuery Not Supported.";
				break;
	}
	return 0;
	
	
}
void update(string sql)
{
	
	string w="where";
	string s="set";
	if(sql.find(s) != string::npos)
	{
       	
	      	if(sql.find(w) != string::npos)
			{
				istringstream iss(sql);
    			string *word;
       			int n=sql.size();
    			word = new string [n];
    			for (int i=0;iss!='\0';i++)
    			{
    				iss>>word[i];
				}
				cout<<"\nQuery Executed Successfully";
				sleep(3);
				cout<<"\nThis Query is performing the Update operation in table "<<word[1]<<" of those records which meet the given condition "<<word[5]<<" as follows  "<<word[3];			
			}
			else
			{	
				char ch;
				cout<<"\nYour Update Query does not have the 'WHERE' clause in it.";
				cout<<"\nPress Y to proceed. ";
				cin>>ch;
				if(ch=='y'||ch=='Y')
				{
					istringstream iss(sql);
    				string *word;
       				int n=sql.size();
    				word = new string [n];
    				for (int i=0;iss!='\0';i++)
    				{
    					iss >> word[i];
					}
					cout<<"\nQuery Executed Successfully";
					sleep(3);
					cout<<"\n\n\nThis Query is Updating the table "<<word[1]<<" as following "<<word[3];
								
				}
				else if(ch=='n'||ch=='N')
				{
					cout<<"\nQuery Execution Halted";
				}
			}
	
	}
	else 
	{
		cout<<"\n'SET' clause not found";
		cout<<"\nQuery Not Supported";
	}
}
void select(string sql)
{
	string s="select";
	string w="where";
	string f="from";
	int ch=5;
	if(sql.find("*")!=string::npos)
	{
		ch=1;
	}
	else if(sql.find("all")!=string::npos)
	{
		ch=2;
	}
	else if(sql.find("distinct")!=string::npos)
	{
		ch=3;
	}
	else if(sql.find("unique")!=string::npos)
	{
		ch=4;
	}
	else 
	{
		ch=5;
	}
	
    if(sql.find("from")!=string::npos)
    {
			    	
	     if(sql.find("where")!=string::npos)
	      {
		
		    istringstream iss(sql);
		    string *word;
		    int n=sql.size();
		    word = new string [n];
		    for(int i=0;iss!='\0';i++)
            {
             
			  iss>> word[i];    	
			
			}	
			
			cout<<"\nQuery Executed Successfully";
			sleep(3);
			switch (ch)
			{
			
				case 1: cout<<"\n\n\nThis Query is printing all the columns from table "<<word[3]<<" of those records which meets the given condition "<<word[5]<<" ";			
						break;
				case 2: cout<<"\n\n\nThis Query is printing all the columns from table "<<word[3]<<" of those records which meets the given condition "<<word[5]<<" ";			
						break;
				case 3: cout<<"\n\n\nThis Query is printing all the different values of column "<<word[2]<<" of those records which meets the given condition "<<word[6]<<" ";
						break;
				case 4: cout<<"\n\n\nThis Query is printing all the different values of column "<<word[2]<<" of those records which meets the given condition "<<word[6]<<" ";
						break;
				case 5: cout<<"\n\n\nThis Query is printing the values of column "<<word[1]<<" from table "<<word[3]<<" of those records which meets the given condition "<<word[5]<<" ";
						break;
			}
		}
		else
		{
		
			char chh;
				cout<<"\nYour Select Query does not have a 'WHERE' clause in it.";
				cout<<"\nPress Y to proceed. ";
				cin>>chh;
				if(chh=='y'||chh=='Y')
				{
					istringstream iss(sql);
		    		string *word;
		    		int n=sql.size();
		    		word = new string [n];
		    		for(int i=0;iss!='\0';i++)
            		{
             			iss>> word[i];    	
					}	
					cout<<"\nQuery Executed Successfully";
					sleep(3);
					switch (ch)
					{
			
						case 1: cout<<"\n\n\nThis Query is printing all the columns from table "<<word[3]<<" ";			
						break;
						case 2: cout<<"\n\n\nThis Query is printing all the columns from table "<<word[3]<<" ";			
						break;
						case 3: cout<<"\n\n\nThis Query is printing all the different values of column "<<word[2]<<" from table "<<word[4
						];
						break;
						case 4: cout<<"\n\n\nThis Query is printing all the different values of column "<<word[2]<<" from table "<<word[4];
						break;
						case 5: cout<<"\n\n\nThis Query is printing the values of column "<<word[1]<<" from table "<<word[3]<<" ";
						break;
					}			
				}
				else if(chh=='n'||chh=='N')
				{
					cout<<"\nQuery Execution Halted";
				}
			
		}
			 
		     
	      
	}
    

}
void delet(string sql)
{
	
	string d="delete";
	string w="where";
	string f="from";
	if(sql.find(f) != string::npos)
	{	
	
		int ff=sql.find(f);
		int dd=sql.find(d);
		if(ff==(dd+7))
		{		
			if(sql.find(w) != string::npos)
			{
				istringstream iss(sql);
    			string *word;
       			int n=sql.size();
    			word = new string [n];
    			for (int i=0;iss!='\0';i++)
    			{
    				iss >> word[i];
				}
				cout<<"\nQuery Executed Successfully";
				sleep(3);
				cout<<"\n\n\nThis Query is performing the Delete operation from table "<<word[2]<<" of those records which meets the given condition "<<word[4]<<" ";			
			}
			else
			{	
				char ch;
				cout<<"\nYour Delete Query does not have a 'WHERE' clause in it.";
				cout<<"\nPress Y to proceed. ";
				cin>>ch;
				if(ch=='y'||ch=='Y')
				{
						istringstream iss(sql);
    					string *word;
       					int n=sql.size();
    					word = new string [n];
    					for (int i=0;iss!='\0';i++)
    					{
    						iss >> word[i];
						}
						cout<<"\nQuery Executed Successfully";
						sleep(3);
						cout<<"\n\n\nThis Query is performing the Delete operation from table "<<word[2];			
			
				}
				else if(ch=='n'||ch=='N')
				{
					cout<<"\nQuery Execution Halted";
				}
			}
		}
		else
		{
			cout<<"\n1Query Not Supported";
		}
	}
	else 
	{
		cout<<"\'FROM' clause not found";
		cout<<"\nQuery Not Supported";
	}
	
	
}
