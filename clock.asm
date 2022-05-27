;----------------------------------------------------------------
;   Tekst zadatka:
;   "Simulirati casovnik sa kazaljkama
;       velika za sate
;       mala za minute
;       najmanja za sekunde
;   i njihovo pomeranje na ekranu."    
;
;   Najbolje pokrenuti u dosbox-u ili necemu slicnom
;   Pokretanje u emu8086 je sporo:
;   - brisanje i pisanje kazaljki traje 1 minut umesto 1 sekund 
;   I moguce je da se neki karakteri ne prikazu dobro
;   (MSDOS extension kvadrat karakter)
;
;   Kratak opis implementacije:
;       Za zadato R, X i Y ce da se nacrta krug 
;       na datim koordinatama
;
;       Zatim ce se nacrtati 3 kazaljke i nakon 1 sekunde
;       ce se obrisati tako sto ce da se nacrtaju ali sa 
;       ' ' karakterom
;
;       Odmah nakom brisanja ce se ugao kazaljke za sekunde
;       smanjiti za 6 stepeni (360/60 - 1 sekunda)
;
;       Kada napravi korak sa 90 na 86 stepeni,
;       znaci da je napravila krug i tada ce se
;       smanjiti ugao kazaljke za minute takodje za 6 stepeni
;       
;       Na isti nacin ce kazaljka za minute da pomeri kazaljku
;       za sate za 30 stepeni kada napravi krug (360/12 - 1 sat)
;
;----------------------------------------------------------------

data segment
    krug_centar_x dw ?      ; centar kruga
    krug_centar_y dw ?      ;
    r dw ?                  ; poluprecnik kruga
    x dw ?                  ; za iteriranje kroz kvadrat oko kruga
    y dw ?                  ;
    
    ; TODO fixaj da se koristi samo krajnja_tacka_xy
    lineBX dw ?        
    lineBY dw ?         
    krajnja_tacka_xy dw ?   ; crtanje linije od A do krajnje tacke
    
    centar_kaz_x dw ?       ; centar sata
    centar_kaz_y dw ?
    
    ; uglovi kazalkji za sat (h), minut (m) i sekund (s) 
    kazaljka_h_ugao dw 0168h    ; 360 stepeni = 0 = 3 sata
    kazaljka_m_ugao dw 05Ah     ; 90 stepeni = 0 minuta
    kazaljka_s_ugao dw 096h     ; 60 stepeni = 5 sekundi
    
    ; duzine kazalkji za sat (h), minut (m) i sekund (s) 
    ; racunaju se na osnovu zadatog poluprecnika kruga
    kazaljka_h_r dw ?
    kazaljka_m_r dw ?
    kazaljka_s_r dw ?
    
    stvarna_brzina db 1 ; realtime ili brzo (1s = 10ms)
    
    pozadina db 010h
    
    komande_txt db  '   KEYBINDS   $'
    s_dugme db      ' S   $'
    s_opis db             '   SPEED$'
    esc_dugme db    ' ESC $'
    ESC_opis db           '    EXIT$'
    komande_kraj db '              $'
    
    ; template za grafiku sa vezbi
    pozX dw ?
    pozY dw ?          ; pozX i pozY koristicemo za pamcenje (X, Y) pozicije trenutnog znaka, 
    adresa_graf dw ?        ; adresa_graf ce sadrzati ofsetnu adresu tog znaka u ekranskoj memoriji.
    sirina dw ?        ; maksimalna sirina i visina
    visina dw ? 
    boja db ?          ; polje boja sadrzace vrednost tekuce boje sa kojom se radi 
    znak db ?          ; polje znak koristicemo kao pomocnu promenljivu u makrou readString.
data ends

;----------------------------------------------------------------

stek segment stack
     dw 128 dup(?)
stek ends

;----------------------------------------------------------------

code segment 

;------------ template za grafiku sa vezbi ----------------------

; ovaj makro se koristi za inicijalizaciju ES registra
macro initGraph
     push ax
     mov ax, 0B800h     ; segmentna adresa_graf graficke memorije postavlja se u ES
     mov es, ax
     mov sirina, 80     ; maksimalna sirina je 80
     mov visina, 25     ; maksimalna visina je 25
     mov pozX, 0        
     mov pozY, 0        
     mov adresa_graf, 0 ; ofsetna adresa_graf prve pozicije je 0
     mov al, pozadina
     mov boja, al       
     pop ax
endm  

; Pomera nas na polje definisano parametrima makroa              
macro setXY x y ;  4, 4 -> adresa_graf = 648
     push ax
     push bx
     push dx
     
	 ; vrednosti se smestaju u pozX i pozY, a nakon toga se racuna adresa_graf tog znaka 
     ; u ekranskoj memoriji po formuli adresa_graf = pozY*160+pozX*2
     mov ax, x
     mov bx, y
     mov pozX, ax
     mov pozY, bx
     mov ax, pozY     
     mov bx, sirina
     shl bx, 1      ; pomeranje bitova u levo za 1: mnozenje sa 2
     mul bx         ; mnozi bx sa ax
     mov bx, pozX   
     shl bx, 1      ; opet pomeranje u levo - mnozenje 
     add ax, bx     ; i na kraju sabiranje
     mov adresa_graf, ax
     
     pop dx
     pop bx
     pop ax
endm

; Postavljanje tekuce boje
macro setBoja b
     mov boja, b
endm  

; Ucitavanje znaka bez prikaza i memorisanja           
keyPress macro
    push ax
    mov ah, 08
    int 21h
    pop ax
endm  

; Ispis znaka na tekucu poziciju ekrana
; setBoja 17
; setXY 4, 4 -> adresa_graf = 648
macro Write c 
     push bx        
     push dx
     
	 mov bx, adresa_graf
     mov es:[bx], c	   ; 0b800h+648 -> 'X'
     mov dl, boja      ; stavljamo boju prvo u pomocni registar dl
     mov es:[bx+1], dl ; 0b800h+649 -> boja se smesta na adresu odmah posle znaka
     
	 pop dx
     pop bx
endm  

; Ispis stringa na ekra
write_string macro str
    LOCAL petlja, kraj    ; definicija dve lokalne petlje - ovo je dobra praksa!
    push ax
    push bx  
    push si               ; index karaktera u stringu
    mov si, 0             ; nulti znak stringa je pocetni
    mov ah, boja          ; trenutno postavljena boja
    mov bx, adresa_graf        ; smestamo trenutnu ofsetnu adresu u bx - ispis se nastavlja tamo gde je adresa_graf!
petlja:
    mov al, str[si]       ; iz stringa se uzimaju znakovi redom i smestaju se u al
    cmp al, '$'           ; ako se naidje na $ prelazi se na kraj ispisa
    je kraj
    mov es:[bx], al       ; iz registra al se zajedno sa bojom stavljaju na ekran
    mov es:[bx+1], ah
    add bx, 2             ; pomeramo se za 2 mesta u memoriji
    add si, 1             ; indeks SI se povecava za 1 - sledeci znak
    jmp petlja
kraj:           

    mov ax, pozX          ; pozicija X se pomera za duzinu stringa
    add ax, si
    mov bx, pozY          ; pozicija Y se upisuje u BX
    setXY ax bx
    pop si
    pop bx
    pop ax
endm 

macro clsColor color
   LOCAL petlja
   push bx
   push cx
   mov bx, 0
   mov cx, 2000
petlja:
   mov es:[bx], ' '     
   mov es:[bx+1], color     
   add bx, 2
   loop petlja
   pop cx
   pop bx
endm

macro cls
    clsColor 7
endm

inttostr proc
   push ax
   push bx
   push cx
   push dx
   push si
   mov bp, sp
   mov ax, [bp+14] 
   mov dl, '$'
   push dx
   mov si, 10
petlja2:
   mov dx, 0
   div si
   add dx, 48
   push dx
   cmp ax, 0
   jne petlja2
   
   mov bx, [bp+12]
petlja2a:      
   pop dx
   mov [bx], dl
   inc bx
   cmp dl, '$'
   jne petlja2a
   pop si  
   pop dx
   pop cx
   pop bx
   pop ax 
   ret 4
inttostr endp  

write_int macro n wi_x wi_y
    push bx
    push ax
 
    push n
    push offset buf
    call inttostr
    mov ax, wi_x
    mov bx, wi_y
    setXY ax bx
    write_string buf

    pop ax
    pop bx
endm

; Kraj programa
krajPrograma macro
    mov ax, 4c02h
    int 21h
endm

;------------- procedure i macro-i za simulaciju sat ------------;


;-------------- procedure za crtanje kruga ----------------------;

;----------------------------------------------------------------;
nacrtajKrug proc
;   prolazak kroz kvadrad 2r*2r sa pocetkom na x-r, y-r
;   racunanje distance od centra x,y za svaki pixel
;   ako je dist == r, tu je kruznica
;
;   2 pixela se tretiraju kao jedan 2x1 kvadrat
;   krug bi inace bio elipsa
;
;   krug_centar_x, krug_centar_y, x i y su tacke izmedju kojih se
;   racuna distanca
;   nisu vezani za memoriju nego vise su 'logicki'
;
;   koriscenje:
;       push x      [+6]
;       push y      [+4]
;       push r      [+2]
;       call nacrtajKrug
;----------------------------------------------------------------;
    push bp
    push di
    push ax
    push si
    push bx
    push cx
    push dx
    
    mov bp, sp
    mov di, 0Eh ; offset do parametara
    
    mov cx, [bp+di +2]
    mov r, cx
    shl cx, 1   ; 2r za iteriranje kroz y kvadrata
    add cx, 1   ; da prodje i kroz zadnji red

    ; koordinati pocetka kvadrata (x,y)
    ; u kom se traze pikseli koju su na kruznici
    ; ax i bx ce biti (x,y) u memoriji
    ; x i y se odnose na kvadrat
    ;
    mov ax, [bp+di +6]  ; centerX
    mov krug_centar_x, ax
    
    mov bx, r           ; r
    sub ax, bx          ; pocetak kvadrata X = krug_centar_x - r
    mov x, ax           ; x u kvadratu (ne u memoriji)
    shl ax, 1           ; jer su pixeli 2x1 (ovo je u memoriji)
    
    mov dx, [bp+di +4]  ; y
    mov krug_centar_y, dx
    sub dx, r           ; y = y - r
    mov y, dx           ; y kvadrata
    
    setXY ax, dx        ; postavljanje x,y, u memoriji
    
    mov bx, adresa_graf  ; pocetak reda kvadrata y
    
    whileY:
        push cx ; sacuvaj y
        push x  ; sacuvaj pocetni x
        
        mov cx, r   ; idi po x
        shl cx, 1   ; 2*r pixela
        add cx, 1
        
        mov si, 0   ; x offset za pristup graf memoriji
        ;add si, r
        
        whileX:
            mov al, b.x
            mov ah, b.krug_centar_x
            mov dl, b.y
            mov dh, b.krug_centar_y
            
            push 0
            push ax
            push dx
            call dist   ; od centra do tacke (x,y) u kvadratu 
            pop ax
            
            cmp ax, r   ; ako je distanca == r, to je kruznica
            jl unutra
            jg okolo
            
            ; kruznica
            mov al, 0Fh             ; bela
            mov ah, 0DBh            ; kvadrat karakter
            jmp boji
            
            okolo:
            mov al, pozadina
            mov ah, ' '
            jmp boji

            unutra:
            mov al, 00h ; crno
            mov ah, ' '
            
            boji:
            
            mov es:[bx+si+1], al   
            mov es:[bx+si]  , ah
            
            mov es:[bx+si+3], al
            mov es:[bx+si+2], ah 

            nastavi:
                        
            add si, 4               ; 2x1 pixel = 4 bajta
            add x, 1                ;
            loop whileX
        
        pop x                       ; pocni ponovo
        pop cx
        add bx, 160                 ; sledeci red
        add y, 1                    ;
        
        loop whileY
    
    pop dx
    pop cx
    pop bx
    pop si
    pop ax
    pop di
    pop bp
    ret 6
nacrtajKrug endp

;----------------------------------------------------------------;
dist proc
;
;   racunanje disatnce izmedju 2 tacke
;   
;   vrednosti koordinata su ogranicene zbog procedure
;   za racunanje kvadratnog korena koja moze da primi
;   samo vrednosti x <= FFh
;
;   razlika izmedju x1 i x2 ne sme biti vise od 11
;   isto vazi i za y1 i y2
;   
;   koriscenje:
;
;       push res    ; [+6]     
;       push x1x2   ; [+4]
;       push y1y2   ; [+2]
;       call dist
;       pop res
;----------------------------------------------------------------;
    push ax
    push bx
    push cx
    push di
    push bp
    
    mov bp, sp
    mov di, 0Ah ; preskakanje registara i povratne adrese 
    
    ; namestanje tako da je x2>x1 i y2>y1
    ; inace ne treba ali mi olaksava racunanje
    ; problem se desi kod negativnih brojeva
    
    mov ax, [bp+di+4]    ; ah = x1, al = x2
    cmp al, ah
    jge neTrebaZamenaX  ; x2 je vec veci
    
    xchg al, ah
    
    neTrebaZamenaX:
    
    mov bx, [bp+di+2]    ; bh = y1, bl = y2
    cmp bl, bh
    jge neTrebaZamenaY  ; y2 je vec veci
 
    xchg bl, bh
 
    neTrebaZamenaY:
    
    sub al, ah  ; al = x2 - x1
    sub bl, bh  ; bl = y2 - y1
    
    mov ah, 0
    mov bh, 0
    
    mul ax      ; ax = (x2 - x1)^2
    mov cx, ax  ; cuvanje ax
    
    mov ax, bx
    mul bx      ; ax = (y2 - y1)^2
    
    add ax, cx  ; ax = (x2 - x1)^2 + (y2 - y1)^2
    
    push 0
    push ax
    call BSisqrt
    pop ax
    
    mov [bp+di+6], ax ; rezultat
    
    pop bp
    pop di
    pop cx
    pop bx
    pop ax
    ret 4
dist endp

;----------------------------------------------------------------;
BSisqrt proc
;   racuna celobrojni kvadratni koren 
;   koristeci binary search algoritam
;       1. predpostavi da je resenje sqrt(x) u sredini (x/2)
;       2. uporedi sredina^2 i x
;       3. ako je x manji, resenje je u donjij polovini (levo)
;          ako je x veci,  resenje je u gornjoj polovini (desno)
;
;   source: https://en.wikipedia.org/wiki/Integer_square_root
;
;   ogranicenje: x <= FFh   
;
;   koriscenje procedure:
;       push 0          ; [+4] ; mesto za rezultat
;       push broj       ; [+2]
;       call BSisqrt
;       pop rezultat
;       
;----------------------------------------------------------------;
    push di
    push ax
    push dx
    push si
    push bp
    
    mov bp, sp
    mov bx, [bp+10 +2] ; +10 registri na steku
    
    mov si, 0   ; L = 0
    mov di, bx
    inc di      ; R = x + 1

    while:
        mov ax, si
        add ax, di ; L + R
        shr ax, 1  ; M = (L + R) / 2
        push ax    ; sacuvaj M

        mul ax     ; M * M
        cmp ax, bx  
        jg  traziLevo   ; M * M >  x, R = M
        jle traziDesno  ; M * M <= x, L = M
        
        traziLevo:
            pop ax      ; M
            mov di, ax  ; R = M
            jmp provera
            
        traziDesno:
            pop ax      ; M
            mov si, ax  ; L = M
            jmp provera
        
        provera:
            mov ax, di
            dec ax
            cmp si, ax
            jne while
        
    mov [bp+10 +4], si ; rezultat
        
    pop bp
    pop si
    pop dx
    pop ax
    pop di
    ret 2
BSisqrt endp

;----------------- procedure za crtanje kazaljke ----------------;

;----------------------------------------------------------------;
nacrtajLiniju proc
;
;   Koristi Brezenhamov algoritam naucen na RG1
;
;   A   - poctna tacka, za inicijalizaciju tacke C
;   B   - krajnja tacka, cuva se na steku
;   C   - ide od A do krajnja_tacka_xy, u CX registru
;   D   - razlika A i krajnja_tacka_xy, u DX registru
;   err - odstupanje od linije za tacku C, u AX registru
;   S   - Sy i Sy su koraci po x i y u zavisnosti od smera linije
;         cuva se u BX registru         
;
;   koriscenje
;       push char_and_color [+10]         
;       push Ax             [+ 8]
;       push Ay             [+ 6]
;       push Bx             [+ 4]
;       push By             [+ 2]
;       call nacrtajLiniju
;
;   Todo
;       Objasni negaciju DY ??
;       citljiviji kod, registri koji imaju smisla
;----------------------------------------------------------------;
    push bp
    push di
    push ax
    push bx
    push dx
    push cx
    push si

    mov bp, sp
    
    mov bl, [bp+14 +2] ; bl = By
    mov bh, [bp+14 +4] ; bh = Bx
    mov cl, [bp+14 +6] ; cl = Ay; Cy na pocetku
    mov ch, [bp+14 +8] ; ch = Ax; Cx na pocetku
    
    mov krajnja_tacka_xy, bx ; b ce biti na steku zbog nedostatka registara

    ; razlika a i b (dx, dy)
    ; dx i dy trebaju biti absolutna vrednost razlike
    ;
    ; u isto vreme racunanje i smera linije
    ; od A do krajnja_tacka_xy (sx, sy)
    ; B moze biti gore levo, dole desno...
    
    ; Dx, Sx
    cmp bh, ch ; cmp Bx, Ax
    jl BxManji
    
    ; Bx Veci
    mov dh, bh
    sub dh, ch
    
    ; x krajnje tacke B je veci od pocetne
    ; znaci da se pomeramo desno, Sx = 1
    ; registar bx se sada koristi za smer S
    mov bh, 1
    jmp gotovX
    
    BxManji:
    mov dh, ch
    sub dh, bh    
    
    mov bh, -1  ; idemo desno
    
    gotovX:
    
    ; Dy, Sy
    cmp bl, cl  ; cmp By, Ay
    jl ByManji 
    
    ; By veci
    mov dl, bl
    sub dl, cl
    
    mov bl, 1   ; idemo dole
    jmp gotovY
    
    ByManji:
    mov dl, cl
    sub dl, bl
    
    mov bl, -1  ; idemo gore
    gotovY:
    neg dl      ; ??
    
    ; u al ce biti greska tj rastojanje od linije
    ; od 3 moguca koraka prema krajnjoj tacki B
    ; idemo onim koji ima najmanje rastojanje od linije
    mov al, dh  
    add al, dl  ; ax = Dx + (-Dy)
    mov ah, al
    
    petlja:
        ; probaj w.ch
        mov b.lineBX, ch
        mov b.lineBY, cl
        setXY lineBX, lineBY  ; pocetak linije (tacka A)
        mov si, adresa_graf
        
        push ax
        mov ax, [bp+14 +10]
        mov es:[si], ah
        mov es:[si+1], al
        pop ax
        
        shl al, 1       ; al = err*2, ah = err

        cmp al, dl      ; cmp err*2, -dy  
        jle neMenjajX
        add ch, bh      ; korak po x
        add ah, dl              
        neMenjajX:
        
        cmp al, dh
        jge neMenjajY
        add cl, bl
        add ah, dh
        neMenjajY:
        
        mov al, ah      ; nova vrednost za err
        
        cmp cx, krajnja_tacka_xy
        jne petlja
    
    pop si
    pop cx
    pop dx
    pop bx
    pop ax
    pop di
    pop bp
    ret 0Ah
nacrtajLiniju endp

;----------------------------------------------------------------;
sin_approx proc
;
;   aproksimira sin(x) gde je x u stepenima u intervalu [0,180]
;   za x>180 vraca negativan znak i sin(x-180)
;
;   y = x*(180-x)/8100
;   
;   posto je rezultat [-1,1] a 8086 nema floating point
;   x se mnozi sa poluprecnikom kruga i time se dobija
;   X koordinata linije pod uglom x i duzine r
;
;   za Y koordinatu se koristi sin_approx(x+90), tj. cos
;
;   izvor:
;   https://scholarworks.umt.edu/cgi/viewcontent.cgi?article=1313&context=tme
;   
;   koriscenje 
;       push 0                [+8] ; mesto za rezultat
;       push 0                [+6] ; mesto za rezultat znak
;       push ugao_stepeni     [+4]
;       push r                [+2]
;       call sin_approx
;       pop znak
;       pop rez
;----------------------------------------------------------------;
    push bp
    push ax
    push bx
    push cx
    push di
    push dx
    
    mov bp, sp
    mov di, 0Ch

    mov cx, [bp+di +4]  ; cx = ugao u stepenima
    
    mov bx, 0B4h        ; bx = 180
    cmp cx, bx          ; da li je u [0, 180]
    jle plus            ; odmah postavi znak i idi dalje
    mov dx, 0
    mov ax, cx
    div bx
    
    mov cx, dx          ; osatak pri deljenju sa 180
    
    and ax, 1           ; ax je rezultat deljenja
    cmp ax, 1           ; ako je rezultat neparan znaci da je
    je  minus           ; ugao u donja 2 kvadranta, znak -
    jne plus            ; paran, gornja 2 kvadranda, znak +
    
    minus:
        mov [bp+di +6], 0FFFFh
        jmp racunanje                
    
    plus:
        mov [bp+di +6], 1
        jmp racunanje                
    
    racunanje:
    
    mov ax, [bp+di +2]  ; r
    
    mul cx              ; ax = r*x
    
    mov bx, 0B4h        ; 180
    sub bx, cx          ; 180 - x
    
    mul bx              ; ax = r*x * (180-x)
    
    mov bx, 01FA4h      ; 8100
    div bx              ; ax / 8100    
    
    mov [bp+di +8], ax
    
    pop dx
    pop di
    pop cx
    pop bx
    pop ax
    pop bp
    ret 4
sin_approx endp

;----------------------------------------------------------------;
cos_approx proc
;
; sin_approx(x+90)
;
; koriscenje:
;   push 0              [+8]    ; mesto za rezultat
;   push 0              [+6]    ; mesto za znak
;   push ugao_stepeni   [+4]    
;   push r              [+2]
;   call cos_approx
;   pop znak
;   pop res
;
;----------------------------------------------------------------;
    push bp
    push ax
    
    mov bp, sp
    
    mov ax, [bp+4 +4]   ; ugao
    add ax, 05Ah        ; ugao + 90
    
    push 0
    push 0
    push ax             ; ugao + 90
    push [bp+4 +2]      ; r
    call sin_approx
    pop [bp+4 +6]       ; vrati znak
    pop [bp+4 +8]       ; vrati rez
    
    pop ax
    pop bp
    ret 4
cos_approx endp

;----------------------------------------------------------------;
xy_linije_pod_uglom proc
; 
;   Procedura prima ugao i duzinu linije od A do B
;   a vraca X i Y tacke B ako bi se linija okrenula
;   oko tacke A za dat ugao
;
;   koriscenje:
;       
;       push 0      [+8] ; mesto za rezultat Y
;       push 0      [+6] ; mesto za rezultat X
;       push ugao   [+4]
;       push r      [+2]
;       call proc
;       pop X
;       pop Y
;----------------------------------------------------------------;
    push bp
    push ax
    push di
    push si
    push dx             ; i dx jer se koristi mnozenje
    
    mov bp, sp
    
    mov di, 0Ah         ; za preskakanje reigstara na steku 
    
    push 0
    push 0
    push [bp+di +4]     ; ugao sa steka (parametar)
    push [bp+di +2]     ; r sa steka    (parametar)
    call sin_approx     ; y se dobija sinusom od ugla*r
    pop si
    pop ax
    
    mul si              ; puta znak
    mov [bp+di +8], ax  ; povratna vrednost, Y
    
    push 0
    push 0
    push [bp+di +4]     ; ugao sa steka (parametar)
    push [bp+di +2]     ; r sa steka    (parametar)
    call cos_approx     ; y se dobija kosinusom od ugla*r
    pop si
    pop ax
    
    shl ax, 1           ; 2x1 pixeli
    mul si              ; puta znak
    mov [bp+di +6], ax  ; povratna vrednost, X
    
    pop dx
    pop si
    pop di
    pop ax
    pop bp
    ret 4
xy_linije_pod_uglom endp
  
;----------------------------------------------------------------;
smanji_ugao macro ugao vrednost
;   
;   Smanjuje ugao za vrednost i ako ugao <= 0
;   stavi se na 360
;   Nikad nece otici ispod jer se uvek umanjuje za
;   umnozak od 6, a uglovi stu takodje umnozci od 6
;----------------------------------------------------------------;
    push ax
    
    LOCAL i_dalje_u_opsegu
    
    mov ax, vrednost
    sub ugao, ax        ; ugao -= vrednost
    mov ax, 00h 
    cmp ugao, ax        ; ugao == 0 ?
    jg i_dalje_u_opsegu
    mov ax, 0168h
    mov ugao, ax        ; if ugao == 0, ugao =  360
    i_dalje_u_opsegu:   ; if ugao > 0, tako ostaje
    
    pop ax
endm

;-------------------- sleep makroi ------------------------------;

;----------------------------------------------------------------;
sleep_1s macro
;
;   Wait interupt, cekanje u mikro sekundama
;   F4240 = milion mikro sekundi = 1 sekund  
;----------------------------------------------------------------;
    push cx
    
    mov ah, 086h
 
    mov cx, 0Fh
    mov dx, 04240h
    
    int 015h
    
    pop cx
endm

;----------------------------------------------------------------;
sleep_10ms macro
;
;   Wait interupt, cekanje u mikro sekundama
;   2719 = 10k mikro sekundi = 0.01 sekund
;----------------------------------------------------------------;
    push cx
    
    mov ah, 086h
 
    mov cx, 0
    mov dx, 02710h
    
    int 015h
    
    pop cx
endm


;----------- Makroi za pozivanje procedura za crtanje -----------;


nacrtajKrugM macro x y r
    push x             ; krug_centar_x
    push y             ; krug_centar_y
    push r             ; r
    call nacrtajKrug
endm

nacrtajLinijuM macro A_x A_y B_x B_y char_and_color
    push char_and_color
    push A_x
    push A_y
    push B_x
    push B_y
    call nacrtajLiniju
endm

nacrtaj_kazaljku macro k_ugao k_r znak_i_boja
    push ax
    push bx
    push cx
    
    push 0
    push 0
    push k_ugao                 ; ugao kazaljke
    push k_r                    ; duzina kazaljke
    call xy_linije_pod_uglom    
    ; procedura na stek stavi X i Y kraja kazaljke pod uglom 
    
    mov cx, znak_i_boja         ; za crtanje kazaljke
    
    mov ax, centar_kaz_x        ; x pocetka kazaljke
    pop bx                      ; x kraja kazaljke
    add ax, bx                  ; x kraja u odnosu na pocetak kaz
    mov dx, ax                  ; cuvanje u dx
    
    mov ax, centar_kaz_y        ; y pocetka kazaljke
    pop bx                      ; y kraja kazaljke
    sub ax, bx                  ; y kraja u odnosu na pocetak kaz
    
    ; nacrtaj kazaljku od pocetka do kraja sa znak_i_boja
    nacrtajLinijuM centar_kaz_x centar_kaz_y dx ax cx
    
    pop cx
    pop bx
    pop ax
endm

nacrtaj_sve_kazaljke macro
    mov bh, 0FEh
    
    mov bl, 04h ; boja kazaljke S                 
    nacrtaj_kazaljku kazaljka_s_ugao kazaljka_s_r bx
    
    mov bl, 0Eh ; boja kazaljke M  
    nacrtaj_kazaljku kazaljka_m_ugao kazaljka_m_r bx
    
    mov bl, 03h ; boja kazaljke H  
    nacrtaj_kazaljku kazaljka_h_ugao kazaljka_h_r bx
endm

;----------------------------------------------------------------
obrisi_sve_kazaljke macro
;   
;   Brisanje se radi tako sato se nacrta kazaljka preko
;   kazaljke ali sa praznik karakterom, pa izgleda kao
;   da se brise
;----------------------------------------------------------------
    mov bh, ' '
    mov bl, 07h 
        
    nacrtaj_kazaljku kazaljka_s_ugao kazaljka_s_r bx
    nacrtaj_kazaljku kazaljka_m_ugao kazaljka_m_r bx
    nacrtaj_kazaljku kazaljka_h_ugao kazaljka_h_r bx
endm

;-------------------- provera user inputa -----------------------

izadji_ako_ESC macro
    proveri_input 01Bh  ; asci od ESC
    cmp ax, 1           ; pritisnut ESC
    je kraj_simulacije
endm

proveri_input macro ascii
    LOCAL pritisnut
    LOCAL nije_pritisnut
    LOCAL kraj_proveri_input
    
    mov ah, 01h         ; citanje buffera tastature
    int 16h            
    
    cmp al, ascii       ; da li je pritisnuto dugme
    jne nije_pritisnut
    
    pritisnut:
    mov ax, 1
    jmp kraj_proveri_input
    
    nije_pritisnut:
    mov ax, 0
    
    kraj_proveri_input:
endm

toggle_brzinu_ako_input_s macro
    proveri_input 073h
    cmp ax, 0
    je ne_menjaj_brzinu
    
    mov al, stvarna_brzina
    cmp al, 0
    je promeni_na_stvarnu
    
    ; stvarna_brzina == 1
    mov al, 0
    mov stvarna_brzina, al
    jmp ne_menjaj_brzinu    ; kraj
    
    promeni_na_stvarnu:
    mov al, 1
    mov stvarna_brzina, al
    
    ne_menjaj_brzinu:
endm

;------------ ispis tabele sa komandama -------------------------

ispis_jedne_komande macro txt_x txt_y dugme opis
    push bx
    
    mov ax, txt_x
    mov bx, txt_y
    setXY ax bx
    
    mov al, 04Eh     ; crvena pozadina, zuti karakter
    setBoja al
    write_string dugme
    
    mov ax, txt_x
    add ax, 5
    
    setXY ax bx
    mov al, 03Eh     ; cyan pozadina, zuti karakter
    setBoja al
    write_string opis
    
    mov al, 04Eh     ; crvena pozadina, zuti karakter
    setBoja al
    Write ' '
    
    pop bx
endm

ispis_komandi macro
    setXY 2 1
    mov al, 04Eh
    setBoja al
    write_string komande_txt
    ispis_jedne_komande 2 2 s_dugme s_opis
    ispis_jedne_komande 2 3 ESC_dugme ESC_opis
    
    setXY               2 4
    setBoja al
    write_string komande_kraj    ; dno tabele
endm

;----------------------------------------------------------------
simuliraj_sat macro sat_x sat_y sat_r
; 
;   Crta sat i simulira kretanje kazaljki u stvarnom vremenu
;   Osnovu prosledjenih X Y i R se racuna velicina i pozicija
;   kruga i kazaljki
;
;   Za 'najmanju' kazaljku se ide kroz petlju iz koje se izadje
;   nakon sto ona napravi krug
;   Na izlazu se pomeri 'veca' kazaljka i proveri da li je ona
;   napravila krug i ako jeste pomeri se najveca i...tako u krug
;----------------------------------------------------------------
    mov ax, sat_x
    mov bx, sat_y
    mov krug_centar_x, ax
    mov krug_centar_y, bx
    
    mov centar_kaz_x, ax
    shl centar_kaz_x, 1     ; mnozi se sa 2 jer su pixeli 2x1
    mov centar_kaz_y, bx
    
    mov ax, sat_r
    mov r, sat_r            ; r sata = r nacrtanog kruga
    
    dec ax                  ; kazaljke za sat je malo manja od r
    mov kazaljka_h_r, ax
    
    dec ax                  ; kazaljke za minut je jos manja od r
    mov kazaljka_m_r, ax    
    
    dec ax                  ; najmanja kazaljka
    mov kazaljka_s_r, ax
    
    nacrtajKrugM krug_centar_x krug_centar_y r
    ispis_komandi
    
    petlja_h:
        petlja_m:
            petlja_s:
                ; provera inputa
                izadji_ako_ESC
                toggle_brzinu_ako_input_s
                
                mov ax, 00C00h
                int 21h             ; praznjenje buffera
                
                ; crtanje kazaljki                                    
                nacrtaj_sve_kazaljke
                
                ; koliko se ceka izmedju pomeranja
                mov al, stvarna_brzina
                xor ah, ah
                
                cmp al, 1
                jne brzo
                sleep_1s    ; stvarna_brzina == 0
                jmp brisanje
                
                brzo:
                sleep_10ms
                
                brisanje:
                obrisi_sve_kazaljke
                smanji_ugao kazaljka_s_ugao 06h
                
                mov ax, 054h            
                cmp ax, kazaljka_s_ugao
                jne petlja_s
                
            smanji_ugao kazaljka_m_ugao 06h
            
            ; ako je dosla do 90 stepeni, prosao je sat
            ; izadji iz petlje i promeni ugao kazaljke za sate
            mov ax, 054h
            cmp ax, kazaljka_m_ugao
            jne petlja_m
            
        smanji_ugao kazaljka_h_ugao 01Eh
        loop petlja_h
endm

start:
    assume cs:code, ss:stek
    mov ax, data
    mov ds, ax        
    
    initGraph
    mov al, pozadina
    clsColor al
    
    simuliraj_sat 014h 0Ch 0Ch ; centar ekrana, r = 12
    
    kraj_simulacije:
        cls
        krajPrograma
code ends
end start
