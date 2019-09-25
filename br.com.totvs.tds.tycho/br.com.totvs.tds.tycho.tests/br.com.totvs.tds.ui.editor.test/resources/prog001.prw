#include "protheus.ch"

user function prog001()
    local n, cResp, cMsg := ""
    local aOpcoes := {}
/* alskdjlasjldlk */
    private cOpcao

    for n := 1 to 5
        aAdd(aOpcoes, strZero(n,1,0))
    next

    n := 0
    while !(cResp == "*")
        tela(aOpcoes)
        n++
        cResp := cOpcao

        if cResp == "1"
            cMsg := "Você escolheu o numero 1"
        elseif cResp == "2"
            cMsg := "Você escolheu o numero 2"
        elseif cResp == "3"
            cMsg := "Você escolheu o numero 3"
        elseif cResp == "4"
            cMsg := "Você escolheu o numero 4"
        elseif cResp == "5"
            cMsg := "Você escolheu o numero 5"
        else
            cMsg := "Nenhum número escolhido"
        endif

        if !empty(cResp)
            if cResp == "2" .or. cResp == "4"
                cMsg += " e é PAR"
            else
                cMsg += " e é IMPAR"
            endif
        endif

        if !(cResp == "*")
            msgAlert(cMsg)
        endif

    enddo

return

static function tela(aaOpcoes)
    Local oDlg,oSay1,oBtn

    if !(valType(aaOpcoes) == "A")
        msgAlerta("Parametro aaOpcoes não é uma lista (array)")
        return cOpcao
    endif

    oDlg := MSDIALOG():Create()
    oDlg:cName := "oDlg"
    oDlg:cCaption := "Escolha um numero"
    oDlg:nLeft := 0
    oDlg:nTop := 0
    oDlg:nWidth := 400
    oDlg:nHeight := 250
    oDlg:lCentered := .T.

    oSay1 := TSAY():Create(oDlg)
    oSay1:cName := "oSay1"
    oSay1:cCaption := "Escolha um número acionando um dos botões abaixo."
    oSay1:nLeft := 10
    oSay1:nTop := 28
    oSay1:nWidth := 250
    oSay1:nHeight := 17
    oSay1:lTransparent := .T.

    oBtn := TButton():Create(oDlg)
    oBtn:cCaption := "<nenhum>"
    oBtn:blClicked := {|| cOpcao := "", oDlg:end() }
    oBtn:nWidth := 90
    oBtn:nTop := 90
    oBtn:nLeft := 10

    oBtn := TButton():Create(oDlg)
    oBtn:cCaption := "<encerrar>"
    oBtn:blClicked := {|| cOpcao := "*", oDlg:end() }
    oBtn:nWidth := 90
    oBtn:nTop := 90
    oBtn:nLeft := 110

    aEval(aaOpcoes, { |x,i| ;
        oBtn := TButton():Create(oDlg),;
        oBtn:cCaption := x,;
        oBtn:blClicked := &("{|| conout('Foi acionado "+x+"'),cOpcao := '"+x+"', oDlg:end() }"),;
        oBtn:nWidth := 30,;
        oBtn:nTop := 60,;
        oBtn:nLeft := (10 * i) + (oBtn:nWidth*(i-1));
        })

//ACTIVATE DIALOG oDlg CENTERED
    oDlg:Activate( oDlg:bLClicked, oDlg:bMoved, oDlg:bPainted,.T.,,,, oDlg:bRClicked, )
//oDlg:Activate()


Return cOpcao
