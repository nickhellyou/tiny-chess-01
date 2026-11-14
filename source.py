import json,os,random,sys
from urllib.request import Request,urlopen
from urllib.error import HTTPError
u=os.environ.get("MAKEFUN_ENV_URL") or sys.exit("no env")
V=[0,100,320,330,500,900,0]
T="kkkkuuuummoqllmpkkkoljiklmmgkkkkaceecgkkekmnelnoeknoelmncgklaceegiiiikkkiklmillmikmmimmmilkkgiiikkkklmmmjkkkjkkkjkkkjkkkjkkkkkklgiijikkkiklljkllkkllillliklkgiijeccaeccaeccaeccageecigggookkoqmkacegegikeioqeiqseiqseioqeekkaeee"
P=[[(ord(T[i*32+j])-107)*5 for j in range(32)] for i in range(7)]
Y=[0,0,10,20,35,55,85,0]
R=random.Random(7)
Z=[[R.getrandbits(64) for _ in range(128)] for _ in range(12)]
A=R.getrandbits(64);F=[R.getrandbits(64) for _ in range(8)];O=[R.getrandbits(64) for _ in range(16)]
Q=[-33,-31,-18,-14,14,18,31,33];X=[-17,-16,-15,-1,1,15,16,17];I=[-17,-15,15,17];L=[-16,-1,1,16]
Kp=[15]*128
SQ=[i for i in range(128) if not i&136]
for s,v in((0,13),(7,14),(4,12),(112,7),(119,11),(116,3)):Kp[s]=v
B=[0]*128;G=[0,0];D=[];J=[]
S=1;C=0;E=-1;H=0;K=0;M=0;N=[0,0];W=[0,0]
n=0;l=0;ab=0;BG=60000
ta=[0]*131072;tb=[0]*131072;tc=[0]*131072;td=[0]*131072;te=[0]*131072
kl=[0]*512;hs=[0]*16384
def at(s,b):
 if b>0:
  if s>16 and(B[s-15]==1 or B[s-17]==1):return 1
 elif s<111 and(B[s+15]==-1 or B[s+17]==-1):return 1
 for o in Q:
  t=s+o
  if not t&136 and B[t]==b*2:return 1
 for o in X:
  t=s+o
  if not t&136 and B[t]==b*6:return 1
 for o in I:
  t=s+o
  while not t&136:
   c=B[t]
   if c:
    if c==b*3 or c==b*5:return 1
    break
   t+=o
 for o in L:
  t=s+o
  while not t&136:
   c=B[t]
   if c:
    if c==b*4 or c==b*5:return 1
    break
   t+=o
 return 0
def gn(co):
 m=[]
 for f in range(128):
  if f&136:continue
  c=B[f]
  if not c or(c>0)!=(S>0):continue
  t=abs(c)
  if t==1:
   d=16*S;r=f>>4;o=f+d
   if not o&136 and not B[o]:
    pr=r==6 if S>0 else r==1
    if pr:
     for p in(5,4,3,2):m.append(f|(o<<7)|(p<<14))
    elif not co:
     m.append(f|(o<<7))
     if((S>0 and r==1)or(S<0 and r==6))and not B[f+2*d]:m.append(f|((f+2*d)<<7))
   for q in(d-1,d+1):
    o=f+q
    if o&136:continue
    v=B[o]
    if v and(v>0)!=(S>0):
     pr=r==6 if S>0 else r==1
     if pr:
      for p in(5,4,3,2):m.append(f|(o<<7)|(p<<14))
     else:m.append(f|(o<<7))
    elif o==E:m.append(f|(o<<7))
  elif t<3 or t==6:
   for o in Q if t==2 else X:
    o2=f+o
    if not o2&136 and(not B[o2]or(B[o2]>0)!=(S>0))and(not co or B[o2]):m.append(f|(o2<<7))
  else:
   for o in I if t==3 else L if t==4 else I+L:
    o2=f+o
    while not o2&136:
     v=B[o2]
     if v:
      if(v>0)!=(S>0):m.append(f|(o2<<7))
      break
     if not co:m.append(f|(o2<<7))
     o2+=o
  if t==6 and not co:
   if S>0:
    if C&1 and not B[5]and not B[6]and B[7]==4 and not at(4,-1)and not at(5,-1)and not at(6,-1):m.append(4|(6<<7))
    if C&2 and not B[3]and not B[2]and not B[1]and B[0]==4 and not at(4,-1)and not at(3,-1)and not at(2,-1):m.append(4|(2<<7))
   else:
    if C&4 and not B[117]and not B[118]and B[119]==-4 and not at(116,1)and not at(117,1)and not at(118,1):m.append(116|(118<<7))
    if C&8 and not B[115]and not B[114]and not B[113]and B[112]==-4 and not at(116,1)and not at(115,1)and not at(114,1):m.append(116|(114<<7))
 return m
def mk(m):
 global S,C,E,H,K,M
 f=m&127;t=m>>7&127;p=m>>14;c=B[f];v=B[t];q=t
 if abs(c)==1 and t==E and not v:q=t-16*S;v=B[q];B[q]=0
 D.append((m,c,v,q,C,E,H,K))
 K^=Z[(abs(c)-1+(c<0)*6)][f]
 if v:K^=Z[(abs(v)-1+(v<0)*6)][q]
 B[t]=p*S if p else c;B[f]=0
 K^=Z[(abs(B[t])-1+(B[t]<0)*6)][t]
 if abs(c)==6:
  G[0 if S>0 else 1]=t
  if abs(t-f)==2:
   if t>f:B[t-1]=B[t+1];B[t+1]=0;K^=Z[3+(S<0)*6][t-1];K^=Z[3+(S<0)*6][t+1]
   else:B[t+1]=B[t-2];B[t-2]=0;K^=Z[3+(S<0)*6][t+1];K^=Z[3+(S<0)*6][t-2]
 o=C;C&=Kp[f]&Kp[t]
 if C!=o:K^=O[o];K^=O[C]
 if E>=0:K^=F[E&7]
 E=f+16*S if abs(c)==1 and abs(t-f)==32 else -1
 if E>=0:K^=F[E&7]
 H=0 if abs(c)==1 or v else H+1
 if v:
  if abs(v)>1:M-=V[abs(v)];N[1 if v<0 else 0]-=1
  else:W[1 if v<0 else 0]-=1
 if p:M+=V[p];N[0 if S>0 else 1]+=1;W[0 if S>0 else 1]-=1
 K^=A;S=-S;J.append(K)
 if at(G[0 if S<0 else 1],S):un();return 0
 return 1
def un():
 global S,C,E,H,K,M
 m,c,v,q,oC,oE,oH,oK=D.pop()
 f=m&127;t=m>>7&127;p=m>>14;w=S<0
 if abs(c)==6 and abs(t-f)==2:
  if t>f:B[t+1]=B[t-1];B[t-1]=0
  else:B[t-2]=B[t+1];B[t+1]=0
 B[f]=c;B[t]=0
 if v:B[q]=v
 if abs(c)==6:G[0 if w else 1]=f
 if v:
  if abs(v)>1:M+=V[abs(v)];N[1 if v<0 else 0]+=1
  else:W[1 if v<0 else 0]+=1
 if p:M-=V[p];N[0 if w else 1]-=1;W[0 if w else 1]+=1
 C,E,H,K=oC,oE,oH,oK;S=-S;J.pop()
def ev():
 a=0;b=0;bp=[0,0];pf=[[0]*8,[0]*8];ps=[[],[]];kg=[0,0];mx=[-1]*8;mn=[8]*8
 for q in SQ:
  c=B[q]
  if not c:continue
  t=abs(c);w=c<0;r=q>>4;f=q&7;i=((7-r)if not w else r)*4+(f if f<4 else 7-f)
  if t==1:
   pf[w][f]+=1;ps[w].append((r,f))
   if w:
    if r>mx[f]:mx[f]=r
   else:
    if r<mn[f]:mn[f]=r
   v=100+P[0][i]
  elif t==6:kg[w]=i;continue
  else:
   v=V[t]+P[t-1][i]
   if t==3:bp[w]+=1
  if w:b+=v
  else:a+=v
 s=a-b
 bk0=[0]*8;bk1=[0]*8
 for f in range(8):
  x=mx[f]
  if f>0 and mx[f-1]>x:x=mx[f-1]
  if f<7 and mx[f+1]>x:x=mx[f+1]
  bk0[f]=x
  x=mn[f]
  if f>0 and mn[f-1]<x:x=mn[f-1]
  if f<7 and mn[f+1]<x:x=mn[f+1]
  bk1[f]=x
 for w in(0,1):
  g=1 if w==0 else -1
  for r,f in ps[w]:
   if pf[w][f]>1:s-=g*12
   if(f==0 or not pf[w][f-1])and(f==7 or not pf[w][f+1]):s-=g*12
   if w==0:
    if bk0[f]<=r:s+=g*Y[r]
   elif bk1[f]>=r:s+=g*Y[7-r]
 if bp[0]==2:s+=25
 if bp[1]==2:s-=25
 h=M if M<6400 else 6400
 s+=(P[5][kg[0]]*h+P[6][kg[0]]*(6400-h))//6400-(P[5][kg[1]]*h+P[6][kg[1]]*(6400-h))//6400
 return s if S>0 else -s
def od(ms,pl,tm):
 sc=[]
 for m in ms:
  f=m&127;t=m>>7&127;p=m>>14
  if m==tm:s=1000000
  else:
   v=B[t]
   if v or p or t==E:s=500000+(10*V[abs(v)]if v else 10)+(V[p]if p else 0)-V[abs(B[f])]//8
   elif m==kl[pl*2]:s=300000
   elif m==kl[pl*2+1]:s=200000
   else:s=hs[m&16383]
  sc.append(s)
 return[m for _,m in sorted(zip(sc,ms),reverse=True)]
def qu(a,b,pl):
 global n,ab
 n+=1
 if n>=l:ab=1;return 0
 if pl>120:return ev()
 w=S>0;ck=at(G[0 if w else 1],-S)
 if ck:bs=-100001;lg=0
 else:
  st=ev()
  if st>=b:return st
  bs=st
  if st>a:a=st
 for m in od(gn(0 if ck else 1),pl,0):
  if not mk(m):continue
  if ck:lg=1
  s=-qu(-b,-a,pl+1);un()
  if ab:return 0
  if s>bs:bs=s
  if s>=b:return s
  if s>a:a=s
 if ck and not lg:return -100000+pl
 return bs
def nm(d,a,b,pl):
 global n,ab,S,C,E,H,K
 if pl and(H>=100 or J.count(K)>1):return 0
 if not(W[0]+W[1])and N[0]+N[1]<2:return 0
 if d<=0:return qu(a,b,pl)
 n+=1
 if n>=l:ab=1;return 0
 w=S>0;ck=at(G[0 if w else 1],-S)
 if ck and pl<40:d+=1
 sl=K&131071;tm=0
 if ta[sl]==K and tb[sl]>=d:
  tm=te[sl];s=td[sl]
  if s>99000:s-=pl
  elif s<-99000:s+=pl
  if tc[sl]==1 or tc[sl]==2 and s>=b or tc[sl]==3 and s<=a:return s
 if d>=3 and not ck and N[0 if w else 1]and ev()>=b:
  D.append((-1,0,0,0,C,E,H,K))
  if E>=0:K^=F[E&7]
  E=-1;H+=1;K^=A;S=-S;J.append(K)
  s=-nm(d-3,-b,-b+1,pl+1)
  _,_,_,_,C,E,H,K=D.pop();S=-S;J.pop()
  if ab:return 0
  if s>=b:return s
 a0=a;bs=-100001;bm=0
 for i,m in enumerate(od(gn(0),pl,tm)):
  if not mk(m):continue
  cp=D[-1][2]
  rd=1 if d>=3 and i>=3 and not cp and not(m>>14)and not ck else 0
  if rd:
   s=-nm(d-2,-b,-a,pl+1)
   if not ab and s>a:s=-nm(d-1,-b,-a,pl+1)
  else:s=-nm(d-1,-b,-a,pl+1)
  un()
  if ab:return 0
  if s>bs:bs=s;bm=m
  if s>a:a=s
  if s>=b:
   if not cp and not(m>>14):
    if kl[pl*2]!=m:kl[pl*2+1]=kl[pl*2];kl[pl*2]=m
    hs[m&16383]+=d*d
   break
 if bs==-100001:return -100000+pl if ck else 0
 ta[sl]=K;tb[sl]=d;te[sl]=bm
 td[sl]=bs+pl if bs>99000 else bs-pl if bs<-99000 else bs
 tc[sl]=3 if bs<=a0 else 2 if bs>=b else 1
 return bs
def rt():
 global n,ab,l
 ms=[]
 for m in gn(0):
  if mk(m):ms.append(m);un()
 if not ms:return []
 sc={m:0 for m in ms}
 n=0;l=BG
 for d in range(1,65):
  ab=0;a=-100001;bb=-100001
  for m in ms:
   mk(m);s=-nm(d-1,-100001,-a,1);un()
   if ab:break
   sc[m]=s
   if s>bb:bb=s
   if s>a:a=s
  if ab:break
  ms.sort(key=lambda m:-sc[m])
  if bb>99000:break
  if n>BG*3//5:break
 return ms
def sf(f):
 global S,C,E,H,K,M
 B[:]=[0]*128;D.clear();J.clear();M=0;N[0]=0;N[1]=0;W[0]=0;W[1]=0
 a=f.split();K=0
 for r,row in enumerate(a[0].split('/')):
  x=0
  for ch in row:
   if ch.isdigit():x+=int(ch)
   else:
    s=(7-r)*16+x;i="PNBRQKpnbrqk".index(ch);c=i+1 if i<6 else -(i-5)
    B[s]=c
    if abs(c)==6:G[0 if c>0 else 1]=s
    elif abs(c)==1:W[0 if c>0 else 1]+=1
    else:M+=V[abs(c)];N[0 if c>0 else 1]+=1
    K^=Z[(abs(c)-1+(c<0)*6)][s]
    x+=1
 S=1 if a[1]=='w' else -1
 C=0
 for ch in a[2]:C|={"K":1,"Q":2,"k":4,"q":8}.get(ch,0)
 E=-1 if a[3]=='-' else(ord(a[3][0])-97)+(ord(a[3][1])-49)*16
 H=int(a[4])if len(a)>4 else 0
 if S<0:K^=A
 if E>=0:K^=F[E&7]
 K^=O[C]
 J.append(K)
def ps(x):
 f=(ord(x[0])-97)+(ord(x[1])-49)*16;t=(ord(x[2])-97)+(ord(x[3])-49)*16
 return f|(t<<7)|(("nbrq".find(x[4])+2 if len(x)>4 else 0)<<14)
def uc(m):
 f=m&127;t=m>>7&127
 return "abcdefgh"[f&7]+str((f>>4)+1)+"abcdefgh"[t&7]+str((t>>4)+1)+("nbrq"[(m>>14)-2]if m>>14 else "")
def po(p,b):
 with urlopen(Request(u+p,data=json.dumps(b).encode(),headers={"content-type":"application/json"}))as r:return json.load(r)
def rn():
 while 1:
  g=po("/game",{})
  if g["status"]=="done":break
  while g["status"]=="your_move":
   sf(g["start_fen"])
   for x in g["moves"]:
    if not mk(ps(x)):sys.exit("replay "+x)
   ms=rt()
   if not ms:ms=[ps(x)for x in g["legal_moves"]]
   ok=0
   for m in ms:
    try:g=po("/move",{"game":g["game"],"move":uc(m)});ok=1;break
    except HTTPError:continue
   if not ok:g=po("/resign",{"game":g["game"]})
 print("done",file=sys.stderr)
if __name__=="__main__":rn()
