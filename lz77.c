#include<stdio.h>
#include<stdlib.h>
#include<string.h>

#define pldbg

#define MIN_REF (2) 
#define LEN_BIT (4)
#define POS_BIT (12)
#define HASH_BIT (19) // 17..24
#define POS_END (1<<POS_BIT)
#define LEN_MASK ((1<<LEN_BIT)-1)
#define BUF_MASK ((1<<POS_BIT)-1)
#define TB_LVL (18)
#define RING(x) (((long long)(x))&BUF_MASK)
#define NEXT(l,p) next_tab[(((l)-MIN_REF)<<POS_BIT)|RING(p)]

int ref_pos(char h, char l){return (((((int)h&0xf0)<<4) | ((int)l&0xff))&BUF_MASK);}
int ref_len(char h, char l){return ((int)h&0xf)+MIN_REF+1;}
char ref_h(int pos, int len){return ((pos>>4)&0xf0)|((len-MIN_REF-1)&0xf);}
char ref_l(int pos, int len){return pos&0xff;}
int hash3(char a, char b, char c){
  int ret= a&0xff;
  ret<<= 8;
  ret|= b&0xff;
  ret<<= HASH_BIT-16;
  ret|= c&(HASH_BIT-16);
  return ret&((1<<HASH_BIT)-1);
}
int bl_abs(int p,int i){
  return BUF_MASK&(p-i);
}
int next_tab[POS_END*TB_LVL];
char* entry[1<<HASH_BIT];

void init(){
  int n= 1<<HASH_BIT;
  while(n-->0)
    entry[n]= 0;
  n= POS_END*TB_LVL;
  while(n-->0)
    next_tab[n]= POS_END;
}
void pop_node(int lvl, int i, int* prev){
  int curdn= NEXT(lvl,i);
  *prev+= curdn;
}

char* get_tb_root(char* ch){
  int hash= hash3(ch[0],ch[1],ch[2]);
  char* root=entry[hash];
  if(ch-root>=POS_END){
    NEXT(MIN_REF, RING(ch))= POS_END;
    root=0;
  }
  entry[hash]=ch;
  return root;
}

// (in, out)
int max_match(char* ch, char** pref){
  char* ref= *pref;
  if(ref==0) return 0;
  int len= MIN_REF;
  NEXT(len, RING(ch))= ch-ref;
  int* prev= &NEXT(len, RING(ch));
  int dnext=0;
  while(ch-ref<POS_END && len<TB_LVL){
    if(ref[len]==ch[len]){
      // ref lvlup
      if(NEXT(len, RING(ref))==POS_END)
	NEXT(len+1, RING(ref))=POS_END;
      //pop oldref
      pop_node(len, RING(ref), prev);

      ++len;
      //update prev
      prev= &NEXT(len, RING(ch));
      // ch.next = ch-ref
      NEXT(len, RING(ch))= ch-ref;
      //- dnext=0;
      *pref= ref;
    }else{
      //update prev
      prev= &NEXT(len, RING(ref));
      //- dnext= NEXT(len, RING(ref));
      //- ref-= dnext; 
      ref-= NEXT(len, RING(ref));
    }
  }
  NEXT(len+1, RING(ch))= POS_END;
  //- ref+= dnext;
  //- *pref= ref;
#ifdef pldbg
  ref=*pref;
  if(len>MIN_REF && (ch[len-1]!=ref[len-1]||ch[0]!=ref[0]||ch[2]!=ref[2]))
    printf("dbg check\n");
#endif
  return len;
}
// (in, out, out)
void tb_add(char* ch, int* ppos, int* plen){
  char* ref= get_tb_root(ch);
  int len= max_match(ch, &ref);
  if(ppos==0) return;
  *plen= len;
  *ppos= ch-ref;
  if(len>MIN_REF){
    if(ch[0]!=ref[0])
      printf("dbg 0 %c`%c\n", ref[0],ch[0]);
    int i;
    for(i=1;i<len;++i){
      tb_add(ch+i,0,0);
    }
  }
}

void encode_stream(FILE* fsrc,FILE* fdst, unsigned int txtlen){
  init();
  char tbuf[16];
  char* src;
  char* dst;
  src= (char*)malloc(txtlen);
  dst= (char*)malloc(txtlen+txtlen>>2);
  unsigned int srclen= fread(src,1,txtlen,fsrc);
  char* psrc= src;
  char* pdst= dst;
  char* srcend= psrc+srclen;
  char* ptbuf=tbuf;
  int len;
  int pos;
  int bitmap=1;
  while(psrc < srcend){
    bitmap<<=1;
    tb_add(psrc, &pos, &len);
    if(len>MIN_REF){
      bitmap|=1;
      ptbuf[0]= ref_h(pos,len);
      ptbuf[1]=	ref_l(pos,len);
      ptbuf+=2;
      psrc+=len;
    }else{
      ptbuf[0]=psrc[0];
      ptbuf++;
      psrc++;
    }
#ifdef pldbg
  if((bitmap&1)!=0){
    int dbgl= ref_len(ptbuf[-2],ptbuf[-1]);
    int dbgd= ref_pos(ptbuf[-2],ptbuf[-1]);
    if(pos!=dbgd || len!=dbgl)
      printf("dbg encodetest fail num [%d,%d]`[%d,%d]\n", dbgd,pos,dbgl,len );
    if(psrc[-1]!=psrc[-dbgd-1])
      printf("dbg encodetest fail last\n");
    if(psrc[-dbgl]!=psrc[-dbgd-dbgl])
      printf("dbg encodetest fail 0\n");
  }else{
    if(psrc[-1]!=ptbuf[-1])
      printf("dbg encodetest fail 2\n");
  }
#endif
    if(bitmap>=(1<<8)){
      *pdst= bitmap&0xff;
      ++pdst;
      memcpy(pdst,tbuf,ptbuf-tbuf);
      pdst+= ptbuf-tbuf;

      ptbuf= tbuf;
      bitmap=1;
    }
  }
  while(bitmap<(1<<8)) bitmap<<=1;
  (pdst++)[0]= bitmap&0xff;
  memcpy(pdst,tbuf,ptbuf-tbuf);
  pdst+= ptbuf-tbuf;
  fwrite(dst,1,pdst-dst,fdst);
  fflush(fdst);
#ifdef pldbg
  printf("write %d\n",pdst-dst);
#endif
  free(dst);
  free(src);
}

void decode_stream(FILE* fsrc,FILE* fdst, unsigned int txtlen){
#ifdef pldbg
  FILE* fdbg= fopen("enwik8","rb");
  char* tdbg= (char*)malloc(txtlen);
  char* pdbg= tdbg;
  fread(pdbg,1,txtlen,fdbg);
#endif
  init();
  char* src;
  char* dst;
  src= (char*)malloc(txtlen+64);
  dst= (char*)malloc(txtlen+64);
  fread(src,1,txtlen,fsrc);
  char* psrc= src;
  char* pdst= dst;
  char* dst_end= dst+txtlen;
  while(pdst < dst_end){
    //read map
    int bitmap= *psrc++;
    int i;
    for(i=(1<<7);i!=0;i>>=1){
      if((bitmap&i)!=0){
	int tpos= ref_pos(psrc[0],psrc[1]);
	int tlen= ref_len(psrc[0],psrc[1]);
	//printf("[%d:%d]",pos,len);
	char* pref= pdst-tpos;
	//memcpy() // pdst[0..tlen] and pref[0..tlen] maybe cross
	int icopytxt;
	for(icopytxt=0;icopytxt<tlen;icopytxt++)
	  pdst[icopytxt]=pref[icopytxt];
#ifdef pldbg
	if(pdst[tlen-1]!=pdbg[tlen-1])
	{
	  printf("check err1 %c`%d`%d [%d,%d] @%d\n", pdbg[tlen-1], (int)pdst[tlen-1], (int)pdbg[tlen-1], tpos, tlen, pdst-dst);
	  sleep(1);
	}
	if(pdst[0]!=pdbg[0]){
	  printf("check err2 %c`%d`%d [%d,%d] @%d\n",pdbg[0],(int)pdst[0], (int)pdbg[0], tpos, tlen, pdst-dst);
	  sleep(1);
	}
	pdbg+=tlen;
#endif
	pdst+=tlen;
	psrc+=2;
      }else{
	//printf("%c",psrc[0]);
	*pdst=*psrc; 
#ifdef pldbg
	if(*pdst!=*pdbg){
	printf("check err0 %c`%c\n",*pdst, *pdbg);
	sleep(1);
      }
	pdbg++;
#endif
	psrc++;
	pdst++;
      }
    }
  }

  fwrite(dst,1,txtlen,fdst);
  free(src);
  free(dst);
}

int main(int argc, const char *argv[])
{
  const char* mode;
  const char* pathsrc;
  const char* pathdst;
  if (argc != 4){
    printf("usage : %s <-c|-d> <src> <dst>\n", argv[0]);
    mode= "-c";
    pathsrc= "enwik8";
    pathdst= "cod";
    return 0; //do not use default setting
  }else{
    mode= argv[1];
    pathsrc= argv[2];
    pathdst= argv[3];
  }
  unsigned int txtlen=-1;
  FILE* fsrc= fopen(pathsrc, "rb");
  FILE* fdst= fopen(pathdst, "wb");
  if(fsrc==0||fdst==0){
    printf("invalid file\n");
    return -1;
  }
  if (!strcmp(mode, "-c")){
    fseek(fsrc,0,SEEK_END);
    txtlen= ftell(fsrc);
    fseek(fsrc,0,SEEK_SET);
    fwrite(&txtlen,1,sizeof(txtlen),fdst);
    encode_stream(fsrc, fdst, txtlen);
  }else if (!strcmp(mode, "-d")){
    fread(&txtlen,1,sizeof(txtlen),fsrc);
    decode_stream(fsrc, fdst, txtlen);
  }else{
    printf("invalid parameter\n");
    return -1;
  }
  fflush(fdst);
  fclose(fsrc);
  fclose(fdst);
  return 0;
}
