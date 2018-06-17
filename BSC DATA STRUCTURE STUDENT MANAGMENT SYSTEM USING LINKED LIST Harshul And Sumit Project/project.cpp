#include<iostream>
#include<cstring>
#include<cstdlib>
#include<unistd.h>
#include<stdio.h>
#include<fstream>

using namespace std;

 
class Student
{
  public:
  char name[20];
  char course[30];
  char rollno[8];
  float **society11;
  long long int mobile;
  long int fees;
  int year;
  int society1;
  int society2;
  int society3;
  int society4;
  Student *next;
  
  
  
  Student();
  Student* Next() { return next; };
  void SetNext(Student* aNext) { next = aNext; };
  int getdata();
  void showdata();
  void societyPriority();
  int studentLogin();
  
};
class Admin
{
  Student* head;
  char name[20];
  char password[40];
  
  public:
  
  Admin()
  {
    head = NULL;
  }
  int adminLogin();
  void adminFunction();
  void printOld();
  void printNew();
  void appendList();
  void descendList();
  void societyAssign();
  void searchStudent();
};  
 
Student::Student()
{ 
  for(int i=0;i<30;i++)
  {
      name[i]=0;
      if(i<20)
      course[i]=0;
      if(i<9)
	rollno[i]=0;
  
  }
  fees=0;
  year=0;
  society11 = new float *[8];
  for(int j=0;j<8;j++)
  {
    society11[j] = new float [2];
    switch(j)
    {
      case 0 : 	society11[j][0]=9;
		society11[j][1]=10.5;
		break;
      case 1 :	society11[j][0]=10.5;
		society11[j][1]=12;
		break;
      case 2 :	society11[j][0]=12;
		society11[j][1]=13.5;
		break;
      case 3 :	society11[j][0]=12.5;
		society11[j][1]=14;
		break;
      case 4 :	society11[j][0]=11.5;
		society11[j][1]=12.5;
		break;
      case 5 :	society11[j][0]=1;
		society11[j][1]=2;
		break;
      case 6 :	society11[j][0]=12;
		society11[j][1]=13;
		break;
      case 7 :	society11[j][0]=10;
		society11[j][1]=11.5;
		break;
      
    
  }
}
}
int digit(int &num)
{
  int dig;
  dig=num%10;
  num=num/10;
  return dig;
}
int Student::getdata()
{
  int ch,num,b=0;
  cout<<"\nYear : ";
  cin>>year;
  num=year;
  cout<<"\nCourses Available:\n";
  cout<<"\n Press 1 For Bsc Hons. Computer Science.";
  cout<<"\n Press 2 For Bsc Hons. Electronics.";
  cout<<"\n Press 3 For Bsc Hons. Chemistry.";
  cout<<"\n Press 4 For Bsc Hons. Physics.";
  cout<<"\n Press 5 For Bsc Hons. Maths.";
  cout<<"\n Press 6 For Bsc Hons. English.";
  cout<<"\n\nIn which of the above courses, you want to register: ";
  cin>>ch;
   
  switch(ch)
  {
	    
    
	    case 1: rollno[1]='0'+digit(num);
		    rollno[0]='0'+digit(num);
		    rollno[2]='H';
		    rollno[3]='C';
		    rollno[4]='S';
		    rollno[5]='4';
		    rollno[6]='1';
		    strcpy(course,"Bsc Hons. Computer Science");
		    fees=26000;
		    break;
	    
	    case 2: rollno[1]='0'+digit(num);
		    rollno[0]='0'+digit(num);
		    rollno[2]='H';
		    rollno[3]='E';
		    rollno[4]='A';
		    rollno[5]='4';
		    rollno[6]='1';
		    strcpy(course,"Bsc Hons. Electronics");
		    fees=26000;
		    break;
	    
	    case 3: rollno[1]='0'+digit(num);
		    rollno[0]='0'+digit(num);
		    rollno[2]='H';
		    rollno[3]='C';
		    rollno[4]='H';
		    rollno[5]='4';
		    rollno[6]='1';
		    strcpy(course,"Bsc Hons. Chemistry");
		    fees=18000;
		    break;
	    
	    case 4: rollno[1]='0'+digit(num);
		    rollno[0]='0'+digit(num);
		    rollno[2]='H';
		    rollno[3]='P';
		    rollno[4]='H';
		    rollno[5]='4';
		    rollno[6]='1';
		    strcpy(course,"Bsc Hons. Physics");
		    fees=12000;
		    break;
	    
	    case 5: rollno[1]='0'+digit(num);
		    rollno[0]='0'+digit(num);
		    rollno[2]='H';
		    rollno[3]='M';
		    rollno[4]='A';
		    rollno[5]='4';
		    rollno[6]='1';
		    strcpy(course,"Bsc Hons. Maths");
		    fees=22000;
		    break;
	    
	    case 6: rollno[1]='0'+digit(num);
		    rollno[0]='0'+digit(num);
		    rollno[2]='H';
		    rollno[3]='E';
		    rollno[4]='N';
		    rollno[5]='4';
		    rollno[6]='1';
		    strcpy(course,"Bsc Hons. English");
		    fees=8000;
		    break;
  
	    default :{ cout<<"\nCourse Not Recognised ";
			b=1;
			return 1 ;
		      }
  }
 if(!b)
 {
    rollno[7]='0'+random()%10;
    rollno[8]='0'+random()%10;
    rollno[9]='\0';
    char a;
    cout<<"\nFees to be Paid "<<fees;
    cout<<"\n\nPress Y to allow the following transaction.\n";
    cin>>a;
    if(a=='y'||a=='Y')
    {
      system("clear");
    cout<<"\n--------------------------------------------------------------------------------------------------------------------------------------\n";
    cout<<"\nEnter the Following Details : "; 
    cout<<"\n\nName : ";
    cin.ignore();
    cin.getline(name,20);
    cout<<"\nMobile No. : ";
    cin>>mobile;
    }
  
  return 0;
   
  }
  
}
void Student::societyPriority()
{
  system("clear");
  cout<<"\n\t\t\tChoose Different Societies According To Your Priority : \n";
  cout<<"\n\t\t\tPress 1  For ACM Society \t\t\t\tTimings : 09:00 am - 10:30 am";
  cout<<"\n\t\t\tPress 2  For Yavanika - The Theatre Society \t\tTimings : 10:30 am - 12:00 pm";
  cout<<"\n\t\t\tPress 3  For Raga - The Dance Society \t\t\tTimings : 12:00 pm - 01:30 pm";
  cout<<"\n\t\t\tPress 4  For DUFC - The Football Club \t\t\tTimings : 12:30 pm - 02:00 pm";
  cout<<"\n\t\t\tPress 5  For MasterChef - The Food Society \t\tTimings : 11:30 am - 01:00 pm";
  cout<<"\n\t\t\tPress 6  For Rhythm - The Music Society \t\tTimings : 01:00 pm - 02:00 pm";
  cout<<"\n\t\t\tPress 7  For ZERO-G - The Gaming Society \t\tTimings : 12:00 pm - 01:00 pm";
  cout<<"\n\t\t\tPress 8  For Electronics Club \t\t\t\tTimings : 10:00 am - 11:30 am";
  cout<<"\n\t\t\tPress 9  For YES!+ - The Art Of Living Society \t\tTimings : 08:00 am - 10:00 am";
  cout<<"\n\t\t\tPress 10 For Robotics Club \t\t\t\tTimings : 10:30 am - 12:00 pm";
  cout<<"\n\n\t\t\tEnter Your Priorities : \n";
  cout<<"\n\t\t\t"; cin>>society1; 
  cout<<"\n\t\t\t"; cin>>society2;
  cout<<"\n\t\t\t"; cin>>society3;
  cout<<"\n\t\t\t"; cin>>society4;
}
void Student::showdata()
{
  cout<<endl;
  cout<<"\n\n\t\t\tName     : "<<name;
  cout<<"\n\t\t\tCourse   : "<<course;
  cout<<"\n\t\t\tRoll No. : "<<rollno;
  cout<<"\n\t\t\tMobile   : "<<mobile;
  cout<<"\n\t\t\tYear     : "<<year;
    
}
void Admin::printOld()
{
      
  // Temp pointer
    Student *tmp = head;
    if(tmp->society3==0 && tmp->society4==0)
    {
    // No nodes
	if ( tmp == NULL ) 
	{
	    cout<<"EMPTY" << endl;
	    return;
	}

    // One node in the list
	if ( tmp->Next() == NULL )
	{
	    
		system("clear");
		tmp->showdata();
		
	    cout<<"\n\n\n\t\t\tSociety Priority Set By : "<<tmp->name;
	    cout<<"\n";
	    for(int i=1;i<3;i++)
	    {
	      int a;
	      switch(i)
	      {
		case 1 : a=tmp->society1;
			  break;
		case 2 : a=tmp->society2;
			  break;
		     
	      }
    
	      switch(a)
	      {
		case 1 : 	cout<<"\n\n\t\t\tACM Society \t\t\t\t\tTimings : 09:00 am - 10:30 am";
		break;
		case 2 :	cout<<"\n\n\t\t\tYavanika - The Theatre Society \t\t\tTimings : 10:30 am - 12:00 pm";
		break;
		case 3 :	cout<<"\n\n\t\t\tRaga - The Dance Society \t\t\tTimings : 12:00 pm - 01:30 pm";
		break;
		case 4 : 	cout<<"\n\n\t\t\tDUFC - The Football Club \t\t\tTimings : 12:30 pm - 02:00 pm";
		break;
		case 5 : 	cout<<"\n\n\t\t\tMasterChef - The Food Society \t\t\tTimings : 11:30 am - 01:00 pm";
		break;
		case 6 : 	cout<<"\n\n\t\t\tRhythm - The Music Society \t\t\tTimings : 01:00 pm - 02:00 pm";
		break;
		case 7 : 	cout<<"\n\n\t\t\tZERO-G - The Gaming Society \t\t\tTimings : 12:00 pm - 01:00 pm";
		break;
		case 8 : 	cout<<"\n\n\t\t\tElectronics Club \t\t\t\tTimings : 10:00 am - 11:30 am";
		break;
		case 9 :        cout<<"\n\n\t\t\tYES!+ - The Art Of Living Society \t\tTimings : 08:00 am - 10:00 am";
		break;
		case 10:        cout<<"\n\n\t\t\tRobotics Club \t\t\t\t\tTimings : 10:30 am - 12:00 pm";
		break;
  
	      }
	    }

      
	}
	else 
	{
    // Parse and print the list
	    while ( tmp != NULL )
	    {	
		system("CLEAR");
		tmp->showdata();
		cout<<"\n\nSociety Priority Set By : "<<tmp->name;
		cout<<"\n";
		for(int i=1;i<3;i++)
		{
		  int a;
		  switch(i)
		  {
		  case 1 : a=tmp->society1;
			  break;
		  case 2 : a=tmp->society2;
			  break;
		     
		  }
    
	      switch(a)
	      {
		case 1 : 	cout<<"\nACM Society \t\t\t\t\tTimings : 09:00 am - 10:30 am";
		break;
		case 2 :	cout<<"\nYavanika - The Theatre Society \t\t\tTimings : 10:30 am - 12:00 pm";
		break;
		case 3 :	cout<<"\nRaga - The Dance Society \t\t\tTimings : 12:00 pm - 01:30 pm";
		break;
		case 4 : 	cout<<"\nDUFC - The Football Club \t\t\tTimings : 12:30 pm - 02:00 pm";
		break;
		case 5 : 	cout<<"\nMasterChef - The Food Society \t\t\tTimings : 11:30 am - 01:00 pm";
		break;
		case 6 : 	cout<<"\nRhythm - The Music Society \t\t\tTimings : 01:00 pm - 02:00 pm";
		break;
		case 7 : 	cout<<"\nZERO-G - The Gaming Society \t\t\tTimings : 12:00 pm - 01:00 pm";
		break;
		case 8 : 	cout<<"\nElectronics Club \t\t\t\tTimings : 10:00 am - 11:30 am";
		break;
		case 9 :        cout<<"\nYES!+ - The Art Of Living Society \t\tTimings : 08:00 am - 10:00 am";
		break;
		case 10:        cout<<"\nRobotics Club \t\t\t\t\tTimings : 10:30 am - 12:00 pm";
		break;
  
	      }
	    }
		tmp = tmp->Next();
	    }
    
  
	}
	      
    }
    else
      tmp = tmp->Next();
}

void Admin::printNew()
{
      
  // Temp pointer
    Student *tmp = head;
      if(tmp->society3!=0 && tmp->society4!=0)
      {// No nodes
	if ( tmp == NULL ) {
	  cout<<"EMPTY" << endl;
	  return;
	}

	// One node in the list
	if ( tmp->Next() == NULL ) 
	{
	    tmp->showdata();
	}
	else {
	  // Parse and print the list
	    do {
		  tmp->showdata();
		  tmp = tmp->Next();
		}while ( tmp != NULL );

    
	    }
      }
}

void Admin::appendList()
{
  Student *newNode = new Student();
  if(!newNode->studentLogin())
  {
    newNode->societyPriority();
    newNode->SetNext(NULL);

    // Create a temp pointer
    Student *tmp = head;

    if ( tmp != NULL ) 
    {
      // Nodes already present in the list
      // Parse to end of list
      while ( tmp->Next() != NULL ) 
      {
        tmp = tmp->Next();
      }

      // Point the last node to the new node
      tmp->SetNext(newNode);
    }
    else 
    {
    // First node in the list
    head = newNode;
    }
    cout<<"\n\t\t\tSubmitting Following Details.......\n";
	if(1)  
	{  
	  sleep(2);
	}
    //cout<<"\nAlmost Done...";
	cout<<"\n\t\t\tFinalizing Students's Account.";
    //f1.write((char*)(&),sizeof(newNode));
	cout<<"\nPress Enter to continue.";
    
	if(1)
	{  
	  sleep(2);
	}
	cout<<endl<<endl;
	system("clear");
	cout<<"\n\n\n\n\n\t\t\t\t\t   *******************************************";
	cout<<"\n\t\t\t\t\t   * WELCOME TO DEEN DAYAL UPADHYAYA COLLEGE *";
	cout<<"\n\t\t\t\t\t   *******************************************\n\n\n";
	cout<<"\n\t\t\tStudents Information : ";
  
	  newNode->showdata();
	  cout<<endl;
      
	 
	 
    
  }
  else
  {
      cout<<"\nINVALID INPUT ";
  }
}

/******************************************************************************************************************************************/
//								  MAIN 
/**************************************************************************************************************************************/
int main()
{
  int ch,a;
  char chh;
  Admin ad;
  do
  {	
    system("clear");
    cout<<"\n\t\t\t\t\t\t---------------------------------\n\t\t\t\t\t\t| DEEN DAYAL UPADHAYAYA COLLEGE |\n\t\t\t\t\t\t---------------------------------\n\n";
    cout<<"\n\t\t\t\t\t\tPress 1 For Student LogIn.";
    cout<<"\n\t\t\t\t\t\tPress 2 For Admin LogIn. ";
    cout<<"\n\t\t\t\t\t\tPress 3 To Exit. \n";
    cout<<"\n\n\t\t\t\t\t\tPlease enter your choice : "; 
    cin>>ch;
    switch(ch)
    {
      case 1: system("clear");
	      cout<<"\n\t\t\t\t\t\t---------------------------------\n\t\t\t\t\t\t| DEEN DAYAL UPADHAYAYA COLLEGE |\n\t\t\t\t\t\t---------------------------------\n\n";
	      cout<<"\n\n\t\t\t\t\t\t\t-----------------\n\t\t\t\t\t\t\t| STUDENT PANEL |\n\t\t\t\t\t\t\t-----------------\n\n";
	      ad.appendList(); 
	      break;
    
      case 2: system("clear");
	      cout<<"\n\t\t\t\t\t\t---------------------------------\n\t\t\t\t\t\t| DEEN DAYAL UPADHAYAYA COLLEGE |\n\t\t\t\t\t\t---------------------------------\n\n";
	      cout<<"\n\n\t\t\t\t\t\t     ------------------------\n\t\t\t\t\t\t     | ADMINISTRATION PANEL |\n\t\t\t\t\t\t     ------------------------\n\n";
	      ad.adminFunction();
	      if(!a)
		 return 0;
            
	      break;
      case 3:	
		if(ch==3)
		{ 
		    sleep(2);    
		    
    
		}
		system("clear");
		cout<<"\n\n\n\n\n\n\n\t\t\t\t\t\tProgram Terminated Succesfully.\n\n\n\n\n\n\n\n\n\n";
		exit(0);
      default:cout<<"\nINVALID INPUT ";
    
    }
    cout<<"\n\n\n\n\n\t\t\t\t\tWould You Like To Exit.('Y'es,'N'o).\n";
    cout<<"\n\t\t\t\t\t"; cin>>chh;    
  }while(chh=='N'||chh=='n');
  
  if(chh=='Y'||chh=='y')
  { 
    sleep(2);    
        
  }
  system("clear");
  
  cout<<"\n\n\n\n\n\n\n\t\t\t\t\t\tProgram Terminated Succesfully.\n\n\n\n\n\n\n\n\n\n";
    return 0;
}

/*****************************************************************************************/
int Student::studentLogin()
{
    
    //fstream f1("Student.dat",ios::in|ios::out);
    if(!getdata())
    {	
	return 0;
//    f1.close();
    }
    else
    {	
      
      cout<<"\n\n\n\t\t\t\t\t\tINVALID INPUT ";
      return 1;
    }
}

int Admin::adminLogin()
{	
    int a=0,i=0;
    char c;
    cout<<"\n\n\t\t\t\t\tUserId : ";
    cin>>name;
    cout<<"\n\t\t\t\t\tPassword : ";
    /*while((c=getc())!='\n');
    {
     password[i]=c;
     cout<<"*";
     i++;
      
    }*/
   cin>>password;
    if(!strcmp(name,"harshulkmr"))
    {  
      if(!strcmp(password,"harshulk1234"))
      {
	a=1;
      }
    }
    if(a)
    {
	if(1)
	  sleep(2);
	system("clear");
	cout<<"\n\n\t\t\t\t\t\t\t   ACCESS GRANTED ";
	if(1)
	  sleep(3);
	system("clear");
	cout<<"\n\n\n\t\t\t\t\t\t\tWelcome Harshul Kumar \n\n";
	return 1;  
    }
    else if(!strcmp(name,"sumityadav"))
    {  
      if(!strcmp(password,"sumity96"))
      {
	a=1;
      }
    }
    if(a)
    {
	if(1)
	  sleep(2);
	system("clear");
	cout<<"\n\n\t\t\t\t\t\t\t   ACCESS GRANTED ";
	if(1)
	  sleep(3);
	system("clear");
	cout<<"\n\n\n\t\t\t\t\t\t\tWelcome Sumit Yadav \n\n";
	return 1;  
    }
    else 
    {
      system("clear");
      cout<<"\n\n\n\t\t\t\t\tACCESS DENIED ";
    
    }
     return 0;
      
}
void Admin::searchStudent()
{
  char namee[20];
  cout<<"\n\n\n\t\t\tEnter the Roll No. of the Student : ";
  cin>>namee;
  // Temp pointer
    Student *temp = head;
    // No nodes
    if ( temp == NULL ) 
    {
      cout<<"EMPTY" << endl;
      return ;
    }
    else
    {
    do
    {
	  system("clear");
	  if(strcmp(namee,temp->rollno)==0)
	  { 
	   
	    temp->showdata();
	    
	    cout<<"\n\n\n\t\t\tSociety Priority Set By : "<<temp->name;
	    cout<<"\n";
	    for(int i=1;i<5;i++)
	    {
	      int a;
	      switch(i)
	      {
		case 1 : a=temp->society1;
			  break;
		case 2 : a=temp->society2;
			  break;
		case 3 : a=temp->society3;
			  break;
		case 4 : a=temp->society4;
			  break;
      
		}
    
	      switch(a)
	      {
		case 1 : 	cout<<"\n\t\t\tPress 1  For ACM Society \t\t\t\tTimings : 09:00 am - 10:30 am";
		break;
		case 2 :	cout<<"\n\t\t\tPress 2  For Yavanika - The Theatre Society \t\tTimings : 10:30 am - 12:00 pm";
		break;
		case 3 :	cout<<"\n\t\t\tPress 3  For Raga - The Dance Society \t\t\tTimings : 12:00 pm - 01:30 pm";
		break;
		case 4 : 	cout<<"\n\t\t\tPress 4  For DUFC - The Football Club \t\t\tTimings : 12:30 pm - 02:00 pm";
		break;
		case 5 : 	cout<<"\n\t\t\tPress 5  For MasterChef - The Food Society \t\tTimings : 11:30 am - 01:00 pm";
		break;
		case 6 : 	cout<<"\n\t\t\tPress 6  For Rhythm - The Music Society \t\tTimings : 01:00 pm - 02:00 pm";
		break;
		case 7 : 	cout<<"\n\t\t\tPress 7  For ZERO-G - The Gaming Society \t\tTimings : 12:00 pm - 01:00 pm";
		break;
		case 8 : 	cout<<"\n\t\t\tPress 8  For Electronics Club \t\t\t\tTimings : 10:00 am - 11:30 am";
		break;
		case 9 :        cout<<"\n\t\t\tPress 9  For YES!+ - The Art Of Living Society \t\tTimings : 08:00 am - 10:00 am";
		break;
		case 10:        cout<<"\n\t\t\tPress 10 For Robotics Club \t\t\t\tTimings : 10:30 am - 12:00 pm";
		break;
  
	      }
	    }
	    cout<<"\n\n\n\t\t\tChoose Any Two Societies From The Priority Set By "<<temp->name<<"\n";
	    cout<<"\n\t\t\t"; cin>>temp->society1;
	    cout<<"\n\t\t\t"; cin>>temp->society2;
	    temp->society3=0;
	    temp->society4=0;
	    if(1)
	    {	system("clear");
		cout<<"\n\n\n\n\n\n\n\n\n\n\n\n\n\n\t\t\t\t\t   ******************************************";
		cout<<"\n\t\t\t\t\t   *     SOCIETIES ASSIGNED SUCCESFULLY     *";
		cout<<"\n\t\t\t\t\t   ******************************************\n\n\n";
		cout<<"\n\n\n\n\n\t\t\t\tRedirecting to The Previous Page ";
	    }
	    sleep(2);

	  } 
	    temp = temp->Next();
	}while(temp!= NULL);
         
    }
    
    
}

void Admin::adminFunction()
{
  char ch;
  int a;
    a=adminLogin();
    if(a==1)
    {
      do{
	cout<<"\n\n\t\t\t\t\t\t     ------------------------\n\t\t\t\t\t\t     | ADMINISTRATION PANEL |\n\t\t\t\t\t\t     ------------------------\n\n";
	cout<<"\n\t\t\t\t\tPress 1 To Show All New Students.";
	cout<<"\n\t\t\t\t\tPress 2 To Assign Societies to New Students.";
	cout<<"\n\t\t\t\t\tPress 3 To Show All Students With Their Societies.";
	cout<<"\n\t\t\t\t\tPress 4 To LogOut. ";
	cout<<"\n\n\t\t\t\t\tPlease enter your choice : ";
	cin>>a;
	
	switch(a)
	{
	  
	  case 1: system("clear");
		  cout<<"\n\t\t\tDetails of all new Students : ";
		  printNew();
		  break;
	    
	  case 2: system("clear");
		  cout<<"\n\t\t\tList of New Students : ";
		  printNew();
		  searchStudent();
		  break;
		  
	  case 3: system("clear");
		  printOld();
		  break;
	     
	  case 4: return ; 
	
	  default : {
		      system("clear");
		      cout<<"\n\n\n\t\t\t\tUNRECOGNISABLE INPUT ";
		    }
	}
      
      cout<<"\n\n\n\n\t\t\t\tWould You Like To Go To Home Page..('Y'es,'N'o).\n";
      cout<<"\t\t\t\t";
      cin>>ch;
      if(ch=='n'||ch=='N')
	system("clear");
      }
	while(ch=='N'||ch=='n');
    }
}