# Montagem das figuras dos mapas no QGIS (PyQGIS; rodar no console Python do QGIS).
# Defina a variável de ambiente DIR_DADOS com o caminho da pasta dados/.
# Arquivo original: Análises/Parte 4 - Análise Espacial/mapas_v48b.py

# -*- coding: utf-8 -*-
"""
Mapas da dissertacao - v48 (2a versao): todos os mapas no QGIS, no estilo das
figuras de Moran/Gi*/Scan da v47 (fonte serifada, limites municipais, legenda
embaixo do mapa), com a rosa dos ventos e a escala do QGIS.

  Figuras 10, 11, 14, 15, 18, 19  - taxas (mesmas classes nos tres mapas) e RR
  Figuras 12, 13, 16, 17, 20, 21  - Moran Local, Gi* e estatistica Scan
  Figuras 22, 23, 24, G3, G4, G5  - GWR

Como rodar (QGIS 3.44): Complementos > Terminal Python e, na linha de comando:
    exec(open(r'<caminho deste arquivo>', encoding='utf-8').read())
Para rodar so uma parte:  PARTES = {'lisa'}; exec(open(...).read())

Nada dos projetos originais e alterado: o script cria projetos "(v48)" ao lado
dos originais e grava as figuras nas pastas "Mapas v48".
"""
import os
import time
import traceback
from collections import Counter

import qgis.core as _qc
from qgis.core import (
    Qgis, QgsProject, QgsVectorLayer, QgsCoordinateReferenceSystem, QgsRectangle,
    QgsPrintLayout, QgsLayoutItemMap, QgsLayoutItemLegend, QgsLayoutItemLabel,
    QgsLayoutItemPicture, QgsLayoutItemScaleBar, QgsLayoutPoint, QgsLayoutSize,
    QgsLayoutExporter, QgsTextFormat, QgsTextBufferSettings, QgsLegendStyle,
    QgsFillSymbol, QgsLineSymbol, QgsLinePatternFillSymbolLayer,
    QgsSingleSymbolRenderer, QgsNullSymbolRenderer, QgsCategorizedSymbolRenderer,
    QgsRendererCategory, QgsGraduatedSymbolRenderer, QgsRendererRange,
    QgsSymbolLayerUtils, QgsBasicNumericFormat, QgsMapLayerLegendUtils,
    QgsPalLayerSettings, QgsVectorLayerSimpleLabeling, QgsSimpleLineCallout,
    QgsCallout, QgsProperty, QgsFeature, QgsGeometry, QgsPointXY, QgsField,
    QgsFeatureRequest, QgsVectorFileWriter, QgsExpression, QgsLegendRenderer,
)
try:
    from qgis.core import NULL as NULL_QGIS
except ImportError:
    NULL_QGIS = None
from qgis.PyQt.QtGui import QColor, QFont, QImage, QPainter
from qgis.PyQt.QtCore import Qt, QVariant
from qgis.PyQt.QtWidgets import QStyleOptionGraphicsItem

# ---------------------------------------------------------------------------
# Caminhos
# ---------------------------------------------------------------------------
ANALISES = os.path.join(os.environ.get("DIR_DADOS", "dados"), "Análises")  # pasta dados/ do repositório
P4 = os.path.join(ANALISES, "Parte 4 - Análise Espacial")
P5 = os.path.join(ANALISES, "Parte 5 - Fatores Associados")
SHP_UF = os.path.join(P4, "Shapes", "BR_UF_2024.shp")
SHP_MUN = os.path.join(P4, "Shapes", "BR_Municipios_2024.shp")
GWR_DIR = os.path.join(P5, "Regressão Espacial - Final", "GWR - Mapas", "Mapas QGIS")
SCAN_DIR = os.path.join(P4, "SIM", "Satscan_Chagas")
RES_DIR = os.path.join(P5, "2 - Resultados")
OUT_P4 = os.path.join(P4, "Mapas v48")
OUT_GWR = os.path.join(RES_DIR, "Mapas v48")
for _d in (OUT_P4, OUT_GWR):
    os.makedirs(_d, exist_ok=True)
LOG = os.path.join(OUT_P4, "log_mapas_v48.txt")

PARTES = globals().get("PARTES", {"taxas", "lisa", "gwr"})
DPI = globals().get("DPI", 600)
ITALICO = globals().get("ITALICO", True)      # "Hotspot" em italico na legenda do Gi*

# ---------------------------------------------------------------------------
# Padrao visual
# ---------------------------------------------------------------------------
FONTE = "Times New Roman"
COR_MUN = "175,175,175,255"     # limites municipais
LARG_MUN = "0.05"               # mm
COR_UF = "51,51,51,255"         # limites estaduais
LARG_UF = "0.13"                # mm
CRS = QgsCoordinateReferenceSystem("EPSG:4674")
EXT = QgsRectangle(-74.2, -34.0, -32.2, 5.4)   # Brasil + Fernando de Noronha
MM = Qgis.LayoutUnit.Millimeters

# estilo normal (paineis de ~8 cm, legenda de 8 pt como na v47) e pequeno
# (Figura 24, paineis de ~5,3 cm)
EST_N = dict(letra=12.0, cab=10.0, leg_tit=8.0, leg=8.0, esc=7.0, banda=5.4,
             gap_leg=0.8, norte=(3.6, 4.8), simb=(4.1, 2.2), simb_top=1.2,
             rot_esq=2.0, tit_baixo=0.9, col=4.0, num=8.5)
EST_P = dict(letra=10.0, cab=8.5, leg_tit=6.5, leg=6.5, esc=6.0, banda=4.8,
             gap_leg=0.7, norte=(2.9, 3.9), simb=(3.4, 1.8), simb_top=1.0,
             rot_esq=1.5, tit_baixo=0.7, col=3.0, num=7.0)
MARGEM_BAIXO = 1.0   # mm abaixo da legenda
ENTRE_LINHAS = 1.5   # mm entre as linhas de paineis

_PROJETOS = []


def log(*a):
    s = time.strftime("[%H:%M:%S] ") + " ".join(str(x) for x in a)
    print(s)
    with open(LOG, "a", encoding="utf-8") as f:
        f.write(s + "\n")


def enum(*nomes):
    """Primeiro enum/atributo existente (API nova ou antiga do QGIS)."""
    for n in nomes:
        obj = _qc
        try:
            for p in n.split("."):
                obj = getattr(obj, p)
            return obj
        except AttributeError:
            continue
    raise AttributeError("nenhum de: %s" % (nomes,))


def comp(nome):
    return enum("Qgis.LegendComponent." + nome, "QgsLegendStyle." + nome)


def lado(nome):
    return enum("QgsLegendStyle.Side." + nome, "QgsLegendStyle." + nome)


def fmt_texto(tam, negrito=False, html=False, italico=False):
    f = QgsTextFormat()
    fonte = QFont(FONTE)
    fonte.setBold(negrito)
    fonte.setItalic(italico)
    f.setFont(fonte)
    if negrito:
        try:
            f.setForcedBold(True)
        except Exception:
            pass
    f.setSize(tam)
    f.setSizeUnit(Qgis.RenderUnit.Points)
    f.setColor(QColor(0, 0, 0))
    if html:
        try:
            f.setAllowHtmlFormatting(True)
        except Exception:
            pass
    return f


def simbolo(cor):
    """Poligono com limite municipal cinza-claro (como nas figuras da v47)."""
    return QgsFillSymbol.createSimple({
        "color": cor, "outline_color": COR_MUN, "outline_width": LARG_MUN,
        "outline_width_unit": "MM", "outline_style": "solid", "style": "solid",
        "joinstyle": "round"})


def camada_uf(proj):
    lyr = QgsVectorLayer(SHP_UF, "Unidades federativas", "ogr")
    sym = QgsFillSymbol.createSimple({
        "color": "0,0,0,0", "style": "no", "outline_color": COR_UF,
        "outline_width": LARG_UF, "outline_width_unit": "MM", "joinstyle": "round"})
    lyr.setRenderer(QgsSingleSymbolRenderer(sym))
    proj.addMapLayer(lyr)
    return lyr


def camada_sem_dados(proj, codigos):
    lyr = QgsVectorLayer(SHP_MUN, "Sem dados", "ogr")
    lyr.setSubsetString('"CD_MUN" IN (%s)' % ",".join("'%s'" % c for c in codigos))
    sym = QgsFillSymbol.createSimple({
        "color": "255,255,255,255", "outline_color": COR_MUN,
        "outline_width": LARG_MUN, "outline_width_unit": "MM"})
    hach = QgsLinePatternFillSymbolLayer()
    hach.setLineAngle(45)
    hach.setDistance(0.6)
    hach.setLineWidth(0.08)
    hach.setColor(QColor(90, 90, 90))
    sym.appendSymbolLayer(hach)
    lyr.setRenderer(QgsSingleSymbolRenderer(sym))
    proj.addMapLayer(lyr)
    return lyr


def camada_categorias(proj, uri, nome, campo, classes):
    """classes: lista de (valor, rotulo, cor)."""
    lyr = QgsVectorLayer(uri, nome, "ogr")
    if not lyr.isValid():
        raise RuntimeError("camada invalida: " + uri)
    cats = [QgsRendererCategory(v, simbolo(c), r) for v, r, c in classes]
    lyr.setRenderer(QgsCategorizedSymbolRenderer(campo, cats))
    proj.addMapLayer(lyr)
    return lyr


def contar_categorias(lyr, expressao, classes):
    exp = QgsExpression(expressao)
    ctx = _qc.QgsExpressionContext()
    ctx.appendScopes(_qc.QgsExpressionContextUtils.globalProjectLayerScopes(lyr))
    req = QgsFeatureRequest()
    try:
        req.setFlags(Qgis.FeatureRequestFlag.NoGeometry)
    except Exception:
        req.setFlags(QgsFeatureRequest.NoGeometry)
    cont = Counter()
    for f in lyr.getFeatures(req):
        ctx.setFeature(f)
        cont[exp.evaluate(ctx)] += 1
    previstos = set(v for v, _, _ in classes)
    fora = sum(k for v, k in cont.items() if v not in previstos)
    return " | ".join("%s: %d" % (r, cont.get(v, 0)) for v, r, _ in classes), fora


def escala_mapa(w, h):
    s = min(w / EXT.width(), h / EXT.height())
    return s, (w - EXT.width() * s) / 2.0, (h - EXT.height() * s) / 2.0


def xy(mx, my, w, h, lon, lat):
    s, ox, oy = escala_mapa(w, h)
    return mx + ox + (lon - EXT.xMinimum()) * s, my + oy + (EXT.yMaximum() - lat) * s


def add_rotulo(layout, texto, x, y, tam, negrito=False):
    lab = QgsLayoutItemLabel(layout)
    lab.setText(texto)
    lab.setTextFormat(fmt_texto(tam, negrito))
    try:
        lab.setMarginX(0)
        lab.setMarginY(0)
    except Exception:
        pass
    layout.addLayoutItem(lab)
    lab.adjustSizeToText()
    lab.attemptMove(QgsLayoutPoint(x, y, MM))
    return lab


def cabecalho(layout, x, y, letra, texto, est):
    """Letra em negrito (como na v47) e, se houver, o titulo do painel."""
    l1 = add_rotulo(layout, letra, x, y, est["letra"], True)
    if texto:
        w = l1.sizeWithUnits().width()
        dy = 0.891 * (est["letra"] - est["cab"]) * 0.35278   # alinha as linhas de base
        add_rotulo(layout, "– " + texto, x + w + 1.0, y + dy, est["cab"])


def add_mapa(layout, x, y, w, h, camadas, nome):
    m = QgsLayoutItemMap(layout)
    m.setId(nome)
    layout.addLayoutItem(m)
    m.attemptResize(QgsLayoutSize(w, h, MM))
    m.attemptMove(QgsLayoutPoint(x, y, MM))
    m.setCrs(CRS)
    m.setLayers(camadas)
    m.setKeepLayerSet(True)
    m.setFrameEnabled(False)
    m.setBackgroundEnabled(False)
    m.zoomToExtent(EXT)
    return m


def bordas_legenda(leg, no):
    """Contorno cinza nos quadradinhos da legenda (como na v47)."""
    try:
        nos = leg.model().layerOriginalLegendNodes(no)
        for i, n in enumerate(nos):
            sym = n.symbol() if hasattr(n, "symbol") else None
            if sym is None:
                continue
            s2 = sym.clone()
            for k in range(s2.symbolLayerCount()):
                sl = s2.symbolLayer(k)
                if sl.layerType() == "SimpleFill":
                    sl.setStrokeColor(QColor(100, 100, 100))
                    sl.setStrokeWidth(0.1)
                    sl.setStrokeStyle(Qt.SolidLine)
            QgsMapLayerLegendUtils.setLegendNodeCustomSymbol(no, i, s2)
        leg.model().refreshLayerLegend(no)
    except Exception as e:
        log("   aviso: bordas da legenda nao aplicadas:", repr(e))


def add_legenda(layout, mapa, x, y, entradas, est, colunas=1, html=False):
    leg = QgsLayoutItemLegend(layout)
    layout.addLayoutItem(leg)
    leg.setLinkedMap(mapa)
    leg.setAutoUpdateModel(False)
    raiz = leg.model().rootGroup()
    raiz.removeAllChildren()
    nos = []
    titulo_leg = ""
    for camada, titulo in entradas:
        no = raiz.addLayer(camada)
        if titulo and not titulo_leg:
            # o titulo da camada vira o titulo da legenda, acima de todas as
            # colunas (assim as duas colunas comecam na mesma altura)
            titulo_leg = titulo
            QgsLegendRenderer.setNodeLegendStyle(no, comp("Hidden"))
        elif titulo:
            no.setCustomProperty("legend/title-label", titulo)
        nos.append(no)
    leg.setTitle(titulo_leg)
    try:
        leg.setTitleAlignment(Qt.AlignLeft)
    except Exception:
        pass
    for nome in ("Title", "Group", "Subgroup"):
        leg.rstyle(comp(nome)).setTextFormat(fmt_texto(est["leg_tit"]))
    leg.rstyle(comp("Title")).setMargin(lado("Top"), 0.0)
    leg.rstyle(comp("Title")).setMargin(lado("Bottom"), est["tit_baixo"])
    leg.rstyle(comp("SymbolLabel")).setTextFormat(fmt_texto(est["leg"], html=html))
    leg.rstyle(comp("Subgroup")).setMargin(lado("Top"), 0.0)
    leg.rstyle(comp("Subgroup")).setMargin(lado("Bottom"), est["tit_baixo"])
    leg.rstyle(comp("Symbol")).setMargin(lado("Top"), est["simb_top"])
    leg.rstyle(comp("SymbolLabel")).setMargin(lado("Top"), est["simb_top"])
    leg.rstyle(comp("SymbolLabel")).setMargin(lado("Left"), est["rot_esq"])
    leg.setSymbolWidth(est["simb"][0])
    leg.setSymbolHeight(est["simb"][1])
    leg.setBoxSpace(0.0)
    leg.setColumnSpace(est["col"])
    if colunas > 1:
        leg.setColumnCount(colunas)
        leg.setSplitLayer(True)
        leg.setEqualColumnWidth(False)
    leg.setBackgroundEnabled(False)
    leg.setFrameEnabled(False)
    leg.setResizeToContents(True)
    for no in nos:
        bordas_legenda(leg, no)
    leg.updateLegend()
    leg.attemptMove(QgsLayoutPoint(x, y, MM))
    medir_legenda(leg)
    leg.attemptMove(QgsLayoutPoint(x, y, MM))
    return leg


def medir_legenda(leg):
    """O QGIS so calcula o tamanho da legenda depois de desenha-la uma vez;
    desenha num rascunho para saber a altura antes de montar a figura."""
    fmt = getattr(QImage, "Format_ARGB32_Premultiplied", None)
    if fmt is None:
        fmt = QImage.Format.Format_ARGB32_Premultiplied
    img = QImage(64, 64, fmt)
    img.fill(0)
    p = QPainter(img)
    try:
        leg.paint(p, QStyleOptionGraphicsItem(), None)
    except Exception as e:
        log("   aviso: legenda nao medida:", repr(e))
    finally:
        p.end()
    tam = leg.sizeWithUnits()
    if tam.height() < 1.0:
        log("   aviso: altura da legenda nao calculada (%.1f mm)" % tam.height())


def add_norte(layout, mapa, x, y, w, h):
    pic = QgsLayoutItemPicture(layout)
    layout.addLayoutItem(pic)
    caminho = QgsSymbolLayerUtils.svgSymbolNameToPath(
        "arrows/NorthArrow_10.svg", QgsProject.instance().pathResolver())
    try:
        pic.setPicturePath(caminho, Qgis.PictureFormat.SVG)
    except Exception:
        pic.setPicturePath(caminho)
    pic.setSvgFillColor(QColor(255, 255, 255))
    pic.setSvgStrokeColor(QColor(0, 0, 0))
    pic.setSvgStrokeWidth(0.1)
    pic.setLinkedMap(mapa)
    pic.attemptResize(QgsLayoutSize(w, h, MM))
    pic.attemptMove(QgsLayoutPoint(x, y, MM))
    return pic


def add_escala(layout, mapa, x, y, est):
    sb = QgsLayoutItemScaleBar(layout)
    layout.addLayoutItem(sb)
    sb.setStyle("Line Ticks Middle")
    sb.setUnits(Qgis.DistanceUnit.Kilometers)
    sb.setUnitLabel("km")
    try:
        sb.setSegmentSizeMode(Qgis.ScaleBarSegmentSizeMode.Fixed)
    except Exception:
        from qgis.core import QgsScaleBarSettings
        sb.setSegmentSizeMode(QgsScaleBarSettings.SegmentSizeFixed)
    sb.setUnitsPerSegment(500)
    sb.setNumberOfSegments(2)
    sb.setNumberOfSegmentsLeft(0)
    sb.setHeight(1.1)
    sb.setLabelBarSpace(0.8)
    sb.setBoxContentSpace(0.3)
    try:
        sb.setMethod(Qgis.ScaleCalculationMethod.HorizontalAverage)
    except Exception:
        pass
    sb.setTextFormat(fmt_texto(est["esc"]))
    nf = QgsBasicNumericFormat()
    nf.setShowThousandsSeparator(True)
    nf.setThousandsSeparator(".")
    nf.setDecimalSeparator(",")
    nf.setNumberDecimalPlaces(0)
    sb.setNumericFormat(nf)
    ls = QgsLineSymbol.createSimple({"line_color": "0,0,0,255", "line_width": "0.12",
                                     "line_width_unit": "MM", "capstyle": "square",
                                     "joinstyle": "miter"})
    for metodo in ("setLineSymbol", "setDivisionLineSymbol", "setSubdivisionLineSymbol"):
        try:
            getattr(sb, metodo)(ls.clone())
        except Exception:
            pass
    sb.setLinkedMap(mapa)
    try:
        sb.refreshItemSize()
    except Exception:
        pass
    sb.update()
    sb.attemptMove(QgsLayoutPoint(x, y, MM))
    return sb


def painel(layout, x0, y0, w, letra, texto, camadas, entradas_leg, norte_escala,
           est, colunas=1, html=False):
    """Painel no estilo da v47: letra (e titulo) no alto, mapa, legenda embaixo;
    rosa dos ventos e escala no espaco vazio a sudoeste do Brasil.
    Sem titulo, a letra fica no canto noroeste do mapa (vazio), como na v47.
    Devolve a coordenada y (mm) do fim do painel."""
    cabecalho(layout, x0 + 0.4, y0 + 0.3, letra, texto, est)
    my = y0 + (est["banda"] if texto else 0.0)
    h = w * EXT.height() / EXT.width()
    mapa = add_mapa(layout, x0, my, w, h, camadas, "Mapa " + letra)
    yl = my + h + est["gap_leg"]
    leg = add_legenda(layout, mapa, x0 + 1.0, yl, entradas_leg, est, colunas, html)
    if norte_escala:
        nx, ny = xy(x0, my, w, h, -71.8, -15.6)
        add_norte(layout, mapa, nx, ny, *est["norte"])
        ex, ey = xy(x0, my, w, h, -73.8, -28.6)
        add_escala(layout, mapa, ex, ey, est)
    tam = leg.sizeWithUnits()
    return yl + tam.height()


def fim_conteudo(layout):
    """y (mm) do item mais baixo do layout (as coordenadas da cena estao em mm)."""
    fim = 0.0
    for it in layout.items():
        if isinstance(it, _qc.QgsLayoutItem) and not isinstance(it, _qc.QgsLayoutItemPage):
            fim = max(fim, it.sceneBoundingRect().bottom())
    return fim


def novo_layout(proj, nome, largura, altura=300.0):
    lay = QgsPrintLayout(proj)
    lay.initializeDefaults()
    lay.setName(nome)
    lay.pageCollection().page(0).setPageSize(QgsLayoutSize(largura, altura, MM))
    proj.layoutManager().addLayout(lay)
    return lay


def ajustar_altura(layout, largura, altura):
    layout.pageCollection().page(0).setPageSize(QgsLayoutSize(largura, altura, MM))


def exportar(layout, caminho):
    exp = QgsLayoutExporter(layout)
    cfg = QgsLayoutExporter.ImageExportSettings()
    cfg.dpi = DPI
    res = exp.exportToImage(caminho, cfg)
    ok = res == QgsLayoutExporter.Success
    log("   exportado" if ok else "   ERRO na exportacao (%s)" % res, os.path.basename(caminho))
    return ok


def novo_projeto():
    proj = QgsProject()
    proj.setCrs(CRS)
    proj.setEllipsoid("EPSG:7019")
    proj.writeEntryBool("Paths", "/Absolute", False)
    _PROJETOS.append(proj)
    return proj


def salvar(proj, destino):
    log("   projeto salvo" if proj.write(destino) else "   ERRO ao salvar projeto", destino)


def fmt_num(v, dec):
    s = "{:,.{d}f}".format(v, d=dec)
    return s.replace(",", "X").replace(".", ",").replace("X", ".")


# ===========================================================================
# Classes da GWR: mesmas classes, cores e n das figuras da v47
# (Legenda_Classes_*.csv); a partir da 2a classe o rotulo ganha "> "
# ===========================================================================
GWR_CLASSES = {
    'Aguda': {
        'Intercept__msk': [
            ('Não significativo', 'Não significativo', '#FFFFFF'),
            ('-5,0 a -3,7', '-5,0 a -3,7', '#2166AC'),
            ('-3,7 a -0,6', '> -3,7 a -0,6', '#67A9CF'),
        ],
        'pct_indigena_2022__msk': [
            ('Não significativo', 'Não significativo', '#FFFFFF'),
            ('-0,0771 a -0,0389', '-0,0771 a -0,0389', '#2166AC'),
            ('-0,0389 a -0,0127', '> -0,0389 a -0,0127', '#67A9CF'),
            ('-0,0127 a 0,0127', '> -0,0127 a 0,0127', '#CCCCCC'),
            ('0,0127 a 0,0147', '> 0,0127 a 0,0147', '#EF8A62'),
            ('0,0147 a 0,0172', '> 0,0147 a 0,0172', '#B2182B'),
        ],
        'adensamento_medio__msk': [
            ('Não significativo', 'Não significativo', '#FFFFFF'),
            ('0,35 a 2,18', '0,35 a 2,18', '#EF8A62'),
            ('2,18 a 3,04', '> 2,18 a 3,04', '#B2182B'),
        ],
        'pct_nasc_fora_municipio__msk': [
            ('Não significativo', 'Não significativo', '#FFFFFF'),
            ('-0,0139 a -0,0089', '-0,0139 a -0,0089', '#2166AC'),
            ('-0,0089 a -0,0042', '> -0,0089 a -0,0042', '#67A9CF'),
        ],
        'pct_parede_taipa__msk': [
            ('Não significativo', 'Não significativo', '#FFFFFF'),
            ('-0,072 a -0,059', '-0,072 a -0,059', '#2166AC'),
            ('-0,059 a -0,018', '> -0,059 a -0,018', '#67A9CF'),
        ],
        'Local_R2__cls': [
            ('Negativo (ajuste local pior que a média)', 'Negativo', '#D7301F'),
            ('0,004 a 0,185', '0,004 a 0,185', '#F7FBFF'),
            ('0,185 a 0,320', '> 0,185 a 0,320', '#C6DBEF'),
            ('0,320 a 0,410', '> 0,320 a 0,410', '#6BAED6'),
            ('0,410 a 0,485', '> 0,410 a 0,485', '#2171B5'),
            ('0,485 a 0,625', '> 0,485 a 0,625', '#08306B'),
        ],
        'Residuo__cls': [
            ('-6,066 a -0,059', '-6,066 a -0,059', '#2166AC'),
            ('-0,059 a -0,009', '> -0,059 a -0,009', '#D1E5F0'),
            ('-0,009 a 0,009', '> -0,009 a 0,009', '#F7F7F7'),
            ('0,009 a 0,046', '> 0,009 a 0,046', '#FDDBC7'),
            ('0,046 a 12,294', '> 0,046 a 12,294', '#B2182B'),
        ],
    },
    'Cronica': {
        'Intercept__msk': [
            ('Não significativo', 'Não significativo', '#FFFFFF'),
            ('-104 a -70', '-104 a -70', '#2166AC'),
            ('-70 a -33', '> -70 a -33', '#67A9CF'),
        ],
        'pct_quilombola_2022__msk': [
            ('Não significativo', 'Não significativo', '#FFFFFF'),
            ('1,3 a 3,5', '1,3 a 3,5', '#EF8A62'),
            ('3,5 a 4,7', '> 3,5 a 4,7', '#B2182B'),
        ],
        'mapbiomas_desmatamento_media_anual_perc__msk': [
            ('Não significativo', 'Não significativo', '#FFFFFF'),
            ('20 a 57', '20 a 57', '#EF8A62'),
            ('57 a 109', '> 57 a 109', '#B2182B'),
        ],
        'pct_esgoto_inadequado__msk': [
            ('Não significativo', 'Não significativo', '#FFFFFF'),
            ('0,31 a 0,82', '0,31 a 0,82', '#EF8A62'),
            ('0,82 a 1,57', '> 0,82 a 1,57', '#B2182B'),
        ],
        'pct_parede_taipa__msk': [
            ('Não significativo', 'Não significativo', '#FFFFFF'),
            ('-27 a -16', '-27 a -16', '#2166AC'),
            ('-16 a -6', '> -16 a -6', '#67A9CF'),
        ],
        'Local_R2__cls': [
            ('Negativo (ajuste local pior que a média)', 'Negativo', '#D7301F'),
            ('0,001 a 0,120', '0,001 a 0,120', '#F7FBFF'),
            ('0,120 a 0,187', '> 0,120 a 0,187', '#C6DBEF'),
            ('0,187 a 0,370', '> 0,187 a 0,370', '#6BAED6'),
            ('0,370 a 0,816', '> 0,370 a 0,816', '#2171B5'),
            ('0,816 a 0,998', '> 0,816 a 0,998', '#08306B'),
        ],
        'Residuo__cls': [
            ('-169,20 a -3,76', '-169,20 a -3,76', '#2166AC'),
            ('-3,76 a -0,48', '> -3,76 a -0,48', '#D1E5F0'),
            ('-0,48 a 0,48', '> -0,48 a 0,48', '#F7F7F7'),
            ('0,48 a 2,25', '> 0,48 a 2,25', '#FDDBC7'),
            ('2,25 a 1.594,15', '> 2,25 a 1.594,15', '#B2182B'),
        ],
    },
    'Mortalidade': {
        'Intercept__msk': [
            ('Não significativo', 'Não significativo', '#FFFFFF'),
            ('-94 a -40', '-94 a -40', '#2166AC'),
            ('-40 a -11', '> -40 a -11', '#67A9CF'),
            ('-11 a 11', '> -11 a 11', '#CCCCCC'),
            ('11 a 29', '> 11 a 29', '#EF8A62'),
            ('29 a 103', '> 29 a 103', '#B2182B'),
        ],
        'pct_negros__msk': [
            ('Não significativo', 'Não significativo', '#FFFFFF'),
            ('-1,05 a -0,49', '-1,05 a -0,49', '#2166AC'),
            ('-0,49 a -0,24', '> -0,49 a -0,24', '#67A9CF'),
            ('-0,24 a 0,24', '> -0,24 a 0,24', '#CCCCCC'),
            ('0,24 a 0,48', '> 0,24 a 0,48', '#EF8A62'),
            ('0,48 a 1,50', '> 0,48 a 1,50', '#B2182B'),
        ],
        'pct_analfabetismo_15mais__msk': [
            ('Não significativo', 'Não significativo', '#FFFFFF'),
            ('-2,80 a -1,02', '-2,80 a -1,02', '#2166AC'),
            ('-1,02 a -0,46', '> -1,02 a -0,46', '#67A9CF'),
            ('-0,46 a 0,46', '> -0,46 a 0,46', '#CCCCCC'),
            ('0,46 a 0,60', '> 0,46 a 0,60', '#EF8A62'),
            ('0,60 a 1,05', '> 0,60 a 1,05', '#B2182B'),
        ],
        'perc_media_agro_19_21__msk': [
            ('Não significativo', 'Não significativo', '#FFFFFF'),
            ('-0,335 a -0,186', '-0,335 a -0,186', '#2166AC'),
            ('-0,186 a -0,122', '> -0,186 a -0,122', '#67A9CF'),
            ('-0,122 a 0,122', '> -0,122 a 0,122', '#CCCCCC'),
            ('0,122 a 0,318', '> 0,122 a 0,318', '#EF8A62'),
            ('0,318 a 0,737', '> 0,318 a 0,737', '#B2182B'),
        ],
        'mapbiomas_fogo_media_historica_perc__msk': [
            ('Não significativo', 'Não significativo', '#FFFFFF'),
            ('-1,45 a -0,56', '-1,45 a -0,56', '#2166AC'),
            ('-0,56 a -0,13', '> -0,56 a -0,13', '#67A9CF'),
            ('-0,13 a 0,13', '> -0,13 a 0,13', '#CCCCCC'),
            ('0,13 a 0,39', '> 0,13 a 0,39', '#EF8A62'),
            ('0,39 a 3,27', '> 0,39 a 3,27', '#B2182B'),
        ],
        'pct_nasc_outra_regiao__msk': [
            ('Não significativo', 'Não significativo', '#FFFFFF'),
            ('-6,86 a -1,09', '-6,86 a -1,09', '#2166AC'),
            ('-1,09 a -0,34', '> -1,09 a -0,34', '#67A9CF'),
            ('-0,34 a 0,34', '> -0,34 a 0,34', '#CCCCCC'),
            ('0,34 a 0,50', '> 0,34 a 0,50', '#EF8A62'),
            ('0,50 a 6,64', '> 0,50 a 6,64', '#B2182B'),
        ],
        'CN_local__cls': [
            ('8,2 a 10,0', '8,2 a 10,0', '#2166AC'),
            ('10,0 a 20,0', '> 10,0 a 20,0', '#92C5DE'),
            ('20,0 a 30,0', '> 20,0 a 30,0', '#E0E0E0'),
            ('30,0 a 50,0', '> 30,0 a 50,0', '#F4A582'),
            ('50,0 a 137,5', '> 50,0 a 137,5', '#B2182B'),
        ],
        'Local_R2__cls': [
            ('Negativo (ajuste local pior que a média)', 'Negativo', '#D7301F'),
            ('0,001 a 0,606', '0,001 a 0,606', '#F7FBFF'),
            ('0,606 a 0,760', '> 0,606 a 0,760', '#C6DBEF'),
            ('0,760 a 0,902', '> 0,760 a 0,902', '#6BAED6'),
            ('0,902 a 0,953', '> 0,902 a 0,953', '#2171B5'),
            ('0,953 a 0,995', '> 0,953 a 0,995', '#08306B'),
        ],
        'Residuo__cls': [
            ('-14,76 a -1,01', '-14,76 a -1,01', '#2166AC'),
            ('-1,01 a -0,15', '> -1,01 a -0,15', '#D1E5F0'),
            ('-0,15 a 0,15', '> -0,15 a 0,15', '#F7F7F7'),
            ('0,15 a 0,86', '> 0,15 a 0,86', '#FDDBC7'),
            ('0,86 a 46,11', '> 0,86 a 46,11', '#B2182B'),
        ],
    },
}

# ===========================================================================
# 1) Taxas (Figuras 10, 11, 14, 15, 18, 19) - pendencia 37
#    Mesmas classes nos tres mapas de cada desfecho: quebras naturais de Jenks
#    sobre os valores positivos das tres taxas juntas, com o zero a parte.
# ===========================================================================
TAXAS = {
    "aguda": dict(
        pasta="SINAN", gpkg="TAXAS_CHAGAS_AGUDA_SUAVIZADAS.gpkg",
        camada="TAXAS_CHAGAS_AGUDA_SUAVIZADAS", dec=1,
        quebras=[1.1, 4.0, 10.4, 19.4], vmax=31.902354908778133,
        cores=["#FFFFFF", "#CEEBA3", "#96D386", "#5CB86A", "#27974E", "#006837"],
        titulo_leg="Casos por 100 mil habitantes", figs=("10", "11"),
        projeto="CHAGAS - AGUDA - FIGURAS 10 e 11 (v48).qgz"),
    "cronica": dict(
        pasta="E-SUS", gpkg="MAPA_CHAGAS_CRONICA_SUAVIZADO_2023.gpkg",
        camada="MAPA_CHAGAS_CRONICA_SUAVIZADO_2023", dec=0,
        quebras=[51, 220, 611, 1231], vmax=1950.617283950617093,
        cores=["#FFFFFF", "#B4E1B9", "#67C4BE", "#39A0BF", "#2B70B1", "#253494"],
        titulo_leg="Notificações por 100 mil habitantes", figs=("14", "15"),
        projeto="CHAGAS - CRONICA - FIGURAS 14 e 15 (v48).qgz"),
    "mort": dict(
        pasta="SIM", gpkg="TAXAS_MORTALIDADE_CHAGAS_PRONTAS.gpkg",
        camada="TAXAS_MORTALIDADE_CHAGAS_PRONTAS", dec=1,
        quebras=[5.1, 14.2, 27.0, 50.8], vmax=129.994394664462817,
        cores=["#FFFFFF", "#FFFFB2", "#FDA649", "#F86C31", "#E62F21", "#BD0026"],
        titulo_leg="Óbitos por 100 mil habitantes", figs=("18", "19"),
        projeto="MORT - CHAGAS - FIGURAS 18 e 19 (v48).qgz"),
}
RR_CORES = ["#FFFFFF", "#FCBEA5", "#FB7050", "#D32020", "#67000D"]
RR_FAIXAS = [(0.0, 0.0, "0"), (0.0, 1.0, "> 0 – 1"), (1.0, 2.0, "> 1 – 2"),
             (2.0, 4.0, "> 2 – 4"), (4.0, 1e12, "> 4")]
LARG_TAXA = 78.0     # mm: cada painel vira uma imagem (A e B lado a lado no Word)


def faixas_taxa(cfg):
    q, d = cfg["quebras"], cfg["dec"]
    lims = [0.0] + list(q)
    fx = [(0.0, 0.0, "0")]
    for i in range(len(q) + 1):
        lo = lims[i]
        if i < len(q):
            hi, hi_lab = q[i], q[i]
        else:
            hi, hi_lab = cfg["vmax"] * (1 + 1e-9) + 1e-9, cfg["vmax"]
        fx.append((lo, hi, "> %s – %s" % (fmt_num(lo, d), fmt_num(hi_lab, d))))
    return fx


def requisicao_sem_geometria(lyr, campo):
    req = QgsFeatureRequest()
    try:
        req.setFlags(Qgis.FeatureRequestFlag.NoGeometry)
    except Exception:
        req.setFlags(QgsFeatureRequest.NoGeometry)
    req.setSubsetOfAttributes([campo], lyr.fields())
    return req


def camada_taxa(proj, cfg, campo, nome, faixas, cores):
    uri = os.path.join(P4, cfg["pasta"], cfg["gpkg"]) + "|layername=" + cfg["camada"]
    lyr = QgsVectorLayer(uri, nome, "ogr")
    if not lyr.isValid():
        raise RuntimeError("camada invalida: " + uri)
    faixas_r = [QgsRendererRange(lo, hi, simbolo(c), lab)
                for (lo, hi, lab), c in zip(faixas, cores)]
    lyr.setRenderer(QgsGraduatedSymbolRenderer(campo, faixas_r))
    proj.addMapLayer(lyr)
    n, fora = [0] * len(faixas), 0
    for f in lyr.getFeatures(requisicao_sem_geometria(lyr, campo)):
        v = f[campo]
        if v is None or v == NULL_QGIS:
            fora += 1
            continue
        for i, (lo, hi, _) in enumerate(faixas):
            if lo <= v <= hi:
                n[i] += 1
                break
        else:
            fora += 1
    log("   %-30s %s | fora das classes: %d" % (nome, " | ".join(
        "%s: %d" % (lab, k) for (_, _, lab), k in zip(faixas, n)), fora))
    return lyr


def rodar_taxas():
    for chave, cfg in TAXAS.items():
        log("== Taxas:", chave)
        proj = novo_projeto()
        uf = camada_uf(proj)
        fx = faixas_taxa(cfg)
        bruta = camada_taxa(proj, cfg, "taxa_bruta", "Taxa bruta", fx, cfg["cores"])
        rr = camada_taxa(proj, cfg, "R_EXCESS", "Risco relativo", RR_FAIXAS, RR_CORES)
        padr = camada_taxa(proj, cfg, "taxa_padronizada", "Taxa padronizada", fx, cfg["cores"])
        suav = camada_taxa(proj, cfg, "taxa_padronizada_suavizada",
                           "Taxa padronizada e suavizada", fx, cfg["cores"])
        f1, f2 = cfg["figs"]
        figuras = [
            (f1, [("A", "Taxa Bruta", bruta, cfg["titulo_leg"]),
                  ("B", "Risco Relativo", rr, "RR")]),
            (f2, [("A", "Taxa Padronizada por Idade", padr, cfg["titulo_leg"]),
                  ("B", "Taxa Padronizada por Idade e Suavizada", suav, cfg["titulo_leg"])]),
        ]
        for fig, paineis in figuras:
            lays, fins = [], []
            for letra, texto, lyr, tleg in paineis:
                lay = novo_layout(proj, "Figura %s%s" % (fig, letra), LARG_TAXA)
                fins.append(painel(lay, 0, 0, LARG_TAXA, letra, texto, [uf, lyr],
                                   [(lyr, tleg)], True, EST_N))
                lays.append((lay, letra))
            altura = max(max(fins), *[fim_conteudo(l) for l, _ in lays]) + MARGEM_BAIXO
            for lay, letra in lays:          # A e B com a mesma altura
                ajustar_altura(lay, LARG_TAXA, altura)
                exportar(lay, os.path.join(OUT_P4, "Figura_%s%s.png" % (fig, letra)))
        salvar(proj, os.path.join(P4, cfg["pasta"], cfg["projeto"]))


# ===========================================================================
# 2) Moran Local, Gi* e Scan (Figuras 12, 13, 16, 17, 20, 21)
# ===========================================================================
CL_LISA = [("Alto-Alto", "Alto-Alto", "#E31A1C"),
           ("Baixo-Alto", "Baixo-Alto", "#A6CEE3"),
           ("Não Significativo", "Não significativo", "#FFFFFF")]
SIG_LISA = [("p < 0,001 (Significância de 99,9%)", "p < 0,001", "#08306B"),
            ("p < 0,01 (Significância de 99%)", "p < 0,01", "#1F78B4"),
            ("p < 0,05 (Significância de 95%)", "p < 0,05", "#A6CEE3"),
            ("Não Significativo", "Não significativo", "#FFFFFF")]


def classes_gi():
    h = "<i>Hotspot</i>" if ITALICO else "Hotspot"
    return [("Hotspot com 99,9% de Confiança", h + " (99,9% de confiança)", "#D7191C"),
            ("Hotspot com 99% de Confiança", h + " (99% de confiança)", "#FF7F00"),
            ("Hotspot com 95% de Confiança", h + " (95% de confiança)", "#FFFFB2"),
            ("Não Significativo", "Não significativo", "#FFFFFF")]


SCAN_EXPR = ("CASE WHEN \"CLUSTER\" IS NULL THEN 'fora' WHEN \"CLUSTER\" = 1 THEN 'prim' "
             "WHEN \"P_VALUE\" < 0.001 THEN 'sec001' WHEN \"P_VALUE\" < 0.05 THEN 'sec05' "
             "ELSE 'ns' END")
SCAN_CL = {"prim": ("Aglomerado primário (p < 0,001)", "#E31A1C"),
           "sec001": ("Aglomerados secundários (p < 0,001)", "#FDAE61"),
           "sec05": ("Aglomerados secundários (p < 0,05)", "#FFFFBF"),
           "ns": ("Aglomerados não significativos", "#ABD9E8"),
           "fora": ("Fora de aglomerados", "#FFFFFF")}

# posicao dos numeros dos aglomerados (lon, lat) e se ha linha de chamada,
# copiadas das figuras da v47
LISA = {
    "aguda": dict(
        pasta="SINAN", gpkg="mapa_chagas_aguda_espacial_completo_GLOBAL_22.gpkg",
        camada="morbi_chagas_aguda", figs=("12", "13"),
        scan=os.path.join(SCAN_DIR, "Aguda", "mapa_chagas_aguda_satscan_final.gpkg"),
        scan_camada="clusters_satscan_aguda", scan_classes=["prim", "sec001", "sec05", "ns", "fora"],
        projeto="CHAGAS - AGUDA - FIGURAS 12 e 13 (v48).qgz",
        rotulos={1: (-49.94, -0.67, 0), 2: (-48.08, -10.55, 1), 3: (-34.59, -10.48, 1),
                 4: (-40.75, -9.52, 1), 5: (-50.55, -32.65, 1), 6: (-48.69, -14.96, 1),
                 7: (-41.71, -16.14, 1), 8: (-35.70, -15.31, 1), 9: (-34.66, -13.72, 1),
                 10: (-42.06, -21.31, 1), 11: (-52.35, -19.17, 1), 12: (-52.21, -16.00, 1),
                 13: (-47.03, -18.00, 1), 14: (-53.66, -24.43, 1), 15: (-53.80, -28.85, 1),
                 16: (-34.11, -8.75, 1), 17: (-47.58, -30.93, 1), 18: (-34.11, -12.13, 1)}),
    "cronica": dict(
        pasta="E-SUS", gpkg="mapa_chagas_cronica_espacial_completo_GLOBAL_22.gpkg",
        camada="morbi_chagas_cronica", figs=("16", "17"),
        scan=os.path.join(SCAN_DIR, "Cronica", "mapa_chagas_cronica_satscan_final.gpkg"),
        scan_camada="clusters_satscan_cronica", scan_classes=["prim", "sec001", "sec05", "ns", "fora"],
        projeto="CHAGAS - CRONICA - FIGURAS 16 e 17 (v48).qgz",
        rotulos={1: (-43.99, -14.50, 0), 2: (-50.36, -20.29, 1), 3: (-55.55, -27.19, 1),
                 4: (-54.03, -23.04, 1), 5: (-48.62, -21.87, 1), 6: (-51.53, -32.10, 1),
                 7: (-57.81, -30.64, 1)}),
    "mort": dict(
        pasta="SIM", gpkg="mapa_chagas_analise_espacial_4.3_GLOBAL_22.gpkg",
        camada="morbimortalidade_chagas", figs=("20", "21"),
        scan=os.path.join(SCAN_DIR, "mapa_chagas_satscan_final.gpkg"),
        scan_camada="clusters_satscan", scan_classes=["prim", "sec001", "fora"],
        projeto="MORT - CHAGAS - FIGURAS 20 e 21 (v48).qgz",
        rotulos={1: (-47.79, -15.66, 0), 2: (-35.70, -11.94, 1), 3: (-52.91, -24.90, 1),
                 4: (-44.89, -25.60, 1)}),
}


def camada_rotulos(proj, scan, posicoes, arquivo, est):
    partes = {}
    for f in scan.getFeatures():
        k = f["CLUSTER"]
        try:
            k = int(k)
        except Exception:
            continue
        partes.setdefault(k, []).append(QgsGeometry(f.geometry()))
    mem = QgsVectorLayer("Point?crs=EPSG:4674&field=num:string(5)&field=lx:double"
                         "&field=ly:double&field=linha:integer", "Números dos aglomerados", "memory")
    feats = []
    for k, (lx, ly, linha) in sorted(posicoes.items()):
        if k not in partes:
            log("   aviso: aglomerado %d sem municipios na camada do Scan" % k)
            continue
        if linha:
            ancora = QgsGeometry.unaryUnion(partes[k]).pointOnSurface()
        else:
            ancora = QgsGeometry.fromPointXY(QgsPointXY(lx, ly))
        ft = QgsFeature(mem.fields())
        ft.setGeometry(ancora)
        ft.setAttributes([str(k), float(lx), float(ly), int(linha)])
        feats.append(ft)
    mem.dataProvider().addFeatures(feats)
    mem.updateExtents()
    lyr = mem
    caminho = os.path.join(OUT_P4, arquivo)
    try:
        opt = QgsVectorFileWriter.SaveVectorOptions()
        opt.driverName = "GPKG"
        opt.layerName = "rotulos"
        res = QgsVectorFileWriter.writeAsVectorFormatV3(mem, caminho, proj.transformContext(), opt)
        if res[0] == QgsVectorFileWriter.NoError:
            l2 = QgsVectorLayer(caminho + "|layername=rotulos", "Números dos aglomerados", "ogr")
            if l2.isValid():
                lyr = l2
    except Exception as e:
        log("   aviso: rotulos mantidos em camada temporaria:", repr(e))
    s = QgsPalLayerSettings()
    s.fieldName = "num"
    fmt = fmt_texto(est["num"], negrito=True)
    buf = QgsTextBufferSettings()
    buf.setEnabled(True)
    buf.setSize(0.45)
    buf.setSizeUnit(Qgis.RenderUnit.Millimeters)
    buf.setColor(QColor(255, 255, 255))
    fmt.setBuffer(buf)
    s.setFormat(fmt)
    try:
        s.placement = enum("Qgis.LabelPlacement.OverPoint", "QgsPalLayerSettings.OverPoint")
    except Exception:
        pass
    dd = s.dataDefinedProperties()
    dd.setProperty(enum("QgsPalLayerSettings.Property.PositionX", "QgsPalLayerSettings.PositionX"),
                   QgsProperty.fromField("lx"))
    dd.setProperty(enum("QgsPalLayerSettings.Property.PositionY", "QgsPalLayerSettings.PositionY"),
                   QgsProperty.fromField("ly"))
    dd.setProperty(enum("QgsPalLayerSettings.Property.Hali", "QgsPalLayerSettings.Hali"),
                   QgsProperty.fromValue("Center"))
    dd.setProperty(enum("QgsPalLayerSettings.Property.Vali", "QgsPalLayerSettings.Vali"),
                   QgsProperty.fromValue("Half"))
    s.setDataDefinedProperties(dd)
    co = QgsSimpleLineCallout()
    co.setEnabled(True)
    co.setLineSymbol(QgsLineSymbol.createSimple({"line_color": "0,0,0,255", "line_width": "0.2",
                                                 "line_width_unit": "MM"}))
    s.setCallout(co)
    try:
        dd = s.dataDefinedProperties()
        dd.setProperty(enum("QgsPalLayerSettings.Property.CalloutDraw",
                            "QgsPalLayerSettings.CalloutDraw"),
                       QgsProperty.fromExpression('"linha" = 1'))
        s.setDataDefinedProperties(dd)
    except Exception as e:
        log("   aviso: linha de chamada sem regra:", repr(e))
    try:
        s.placementSettings().setOverlapHandling(Qgis.LabelOverlapHandling.AllowOverlapAtNoCost)
    except Exception:
        s.displayAll = True
    try:
        s.obstacleSettings().setIsObstacle(False)
    except Exception:
        pass
    lyr.setLabeling(QgsVectorLayerSimpleLabeling(s))
    lyr.setLabelsEnabled(True)
    lyr.setRenderer(QgsNullSymbolRenderer())
    proj.addMapLayer(lyr)
    log("   numeros dos aglomerados:", len(feats))
    return lyr


def rodar_lisa():
    for chave, cfg in LISA.items():
        log("== Moran, Gi* e Scan:", chave)
        proj = novo_projeto()
        uf = camada_uf(proj)
        uri = os.path.join(P4, cfg["pasta"], cfg["gpkg"]) + "|layername=" + cfg["camada"]
        cl = camada_categorias(proj, uri, "Moran Local (LISA)", "LISA_CLUSTER", CL_LISA)
        sig = camada_categorias(proj, uri, "Significância do Moran Local", "LISA_SIGNIFICANCIA", SIG_LISA)
        gi = camada_categorias(proj, uri, "Getis-Ord Gi*", "GETIS_HOTSPOT", classes_gi())
        base = QgsVectorLayer(uri, "Base municipal", "ogr")
        base.setRenderer(QgsSingleSymbolRenderer(simbolo("#FFFFFF")))
        proj.addMapLayer(base)
        sc_cls = [(k, SCAN_CL[k][0], SCAN_CL[k][1]) for k in cfg["scan_classes"]]
        scan = camada_categorias(proj, cfg["scan"] + "|layername=" + cfg["scan_camada"],
                                 "Estatística Scan (SaTScan)", SCAN_EXPR, sc_cls)
        for lyr, campo, cls in ((cl, '"LISA_CLUSTER"', CL_LISA), (sig, '"LISA_SIGNIFICANCIA"', SIG_LISA),
                                (gi, '"GETIS_HOTSPOT"', classes_gi()), (scan, SCAN_EXPR, sc_cls)):
            txt, fora = contar_categorias(lyr, campo, cls)
            log("   %-30s %s | fora das classes: %d" % (lyr.name(), txt, fora))
        rot = camada_rotulos(proj, scan, cfg["rotulos"], "rotulos_scan_%s.gpkg" % chave, EST_N)
        f1, f2 = cfg["figs"]
        lay = novo_layout(proj, "Figura " + f1, 160.0)
        fa = painel(lay, 0, 0, 78, "A", None, [uf, cl], [(cl, "Moran Local (LISA)")], True, EST_N)
        fb = painel(lay, 82, 0, 78, "B", None, [uf, sig], [(sig, "Significância do Moran Local")], True, EST_N)
        ajustar_altura(lay, 160.0, max(fa, fb, fim_conteudo(lay)) + MARGEM_BAIXO)
        exportar(lay, os.path.join(OUT_P4, "Figura_%s.png" % f1))
        lay = novo_layout(proj, "Figura " + f2, 160.0)
        fa = painel(lay, 0, 0, 78, "A", None, [uf, gi], [(gi, "Getis-Ord Gi*")], True, EST_N, html=ITALICO)
        fb = painel(lay, 82, 0, 78, "B", None, [rot, uf, scan, base],
                    [(scan, "Estatística Scan (SaTScan)")], True, EST_N)
        ajustar_altura(lay, 160.0, max(fa, fb, fim_conteudo(lay)) + MARGEM_BAIXO)
        exportar(lay, os.path.join(OUT_P4, "Figura_%s.png" % f2))
        salvar(proj, os.path.join(P4, cfg["pasta"], cfg["projeto"]))


# ===========================================================================
# 3) GWR (Figuras 22, 23, 24, G3, G4, G5) - pendencia 36
# ===========================================================================
SEM_DADOS = {
    "Aguda": ["5101837"],
    "Cronica": ["5101837", "2408201", "2413201", "2605459", "2109452"],
    "Mortalidade": ["5101837", "2408201", "2413201", "2605459", "2109452"],
}
POS = {
    "2x2": [(0, 0), (1, 0), (0, 1), (1, 1)],
    "3x2": [(0, 0), (1, 0), (2, 0), (0, 1), (1, 1), (2, 1)],
    "2+1": [(0, 0), (1, 0), (0.5, 1)],
}
FIGS_GWR = {
    "Aguda": dict(projeto="CHAGAS - AGUDA - GWR - FIGURAS 22 e G3 (v48).qgz", figuras=[
        dict(nome="Figura_22", grade="2x2", paineis=[
            ("A", "População Indígena", "pct_indigena_2022__msk", "Coeficiente Local"),
            ("B", "Adensamento", "adensamento_medio__msk", "Coeficiente Local"),
            ("C", "Naturais de Outro Município", "pct_nasc_fora_municipio__msk", "Coeficiente Local"),
            ("D", "Paredes de Taipa", "pct_parede_taipa__msk", "Coeficiente Local")]),
        dict(nome="Figura_G3", grade="2+1", paineis=[
            ("A", "Intercepto", "Intercept__msk", "Coeficiente Local"),
            ("B", "R² Local", "Local_R2__cls", "R² Local"),
            ("C", "Resíduos do SDM Parcial (Rainha)", "Residuo__cls", "Resíduo")]),
    ]),
    "Cronica": dict(projeto="CHAGAS - CRONICA - GWR - FIGURAS 23 e G4 (v48).qgz", figuras=[
        dict(nome="Figura_23", grade="2x2", paineis=[
            ("A", "População Quilombola", "pct_quilombola_2022__msk", "Coeficiente Local"),
            ("B", "Desmatamento", "mapbiomas_desmatamento_media_anual_perc__msk", "Coeficiente Local"),
            ("C", "Esgotamento Inadequado", "pct_esgoto_inadequado__msk", "Coeficiente Local"),
            ("D", "Paredes de Taipa", "pct_parede_taipa__msk", "Coeficiente Local")]),
        dict(nome="Figura_G4", grade="2+1", paineis=[
            ("A", "Intercepto", "Intercept__msk", "Coeficiente Local"),
            ("B", "R² Local", "Local_R2__cls", "R² Local"),
            ("C", "Resíduos do SAR (Rainha)", "Residuo__cls", "Resíduo")]),
    ]),
    "Mortalidade": dict(projeto="MORT - CHAGAS - GWR - FIGURAS 24 e G5 (v48).qgz", figuras=[
        dict(nome="Figura_24", grade="3x2", paineis=[
            ("A", "População Negra", "pct_negros__msk", "Coeficiente Local"),
            ("B", "Analfabetismo", "pct_analfabetismo_15mais__msk", "Coeficiente Local"),
            ("C", "Agropecuária no VAB", "perc_media_agro_19_21__msk", "Coeficiente Local"),
            ("D", "Fogo", "mapbiomas_fogo_media_historica_perc__msk", "Coeficiente Local"),
            ("E", "Naturais de Outra Região", "pct_nasc_outra_regiao__msk", "Coeficiente Local"),
            ("F", "Número de Condição Local", "CN_local__cls", "Número de Condição")]),
        dict(nome="Figura_G5", grade="2+1", paineis=[
            ("A", "Intercepto", "Intercept__msk", "Coeficiente Local"),
            ("B", "R² Local", "Local_R2__cls", "R² Local"),
            ("C", "Resíduos do SAR (Rainha)", "Residuo__cls", "Resíduo")]),
    ]),
}


def camada_gwr(proj, desfecho, coluna, nome):
    uri = os.path.join(GWR_DIR, desfecho, "Mapas_GWR_%s.gpkg" % desfecho) + "|layername=mapas_gwr"
    classes = GWR_CLASSES[desfecho][coluna]
    lyr = camada_categorias(proj, uri, nome, coluna, classes)
    txt, fora = contar_categorias(lyr, '"%s"' % coluna, classes)
    log("   %-48s %s | fora das classes: %d" % (nome, txt, fora))
    return lyr


def rodar_gwr():
    for desfecho, cfg in FIGS_GWR.items():
        log("== GWR:", desfecho)
        proj = novo_projeto()
        uf = camada_uf(proj)
        sd = camada_sem_dados(proj, SEM_DADOS[desfecho])
        for fig in cfg["figuras"]:
            est = EST_P if fig["grade"] == "3x2" else EST_N
            # largura do mapa e passo entre colunas (4 mm de intervalo; 2 mm na de 3 colunas)
            larg, passo = (52.0, 54.0) if fig["grade"] == "3x2" else (78.0, 82.0)
            lay = novo_layout(proj, fig["nome"].replace("_", " "), 160.0)
            y_linha, linha_atual, fim_anterior, fim_linha = 0.0, 0, 0.0, 0.0
            for k, (letra, texto, coluna, tleg) in enumerate(fig["paineis"]):
                col, lin = POS[fig["grade"]][k]
                if lin != linha_atual:
                    linha_atual = lin
                    fim_anterior, fim_linha = fim_linha, 0.0
                    y_linha = fim_anterior + ENTRE_LINHAS
                lyr = camada_gwr(proj, desfecho, coluna, "%s%s %s" % (
                    fig["nome"].replace("Figura_", ""), letra, coluna))
                fim = painel(lay, col * passo, y_linha, larg, letra, texto, [uf, sd, lyr],
                             [(lyr, tleg), (sd, None)], k == 0, est, colunas=2)
                fim_linha = max(fim_linha, fim)
            ajustar_altura(lay, 160.0, max(fim_linha, fim_conteudo(lay)) + MARGEM_BAIXO)
            exportar(lay, os.path.join(OUT_GWR, fig["nome"] + ".png"))
        salvar(proj, os.path.join(RES_DIR, cfg["projeto"]))


def principal():
    t0 = time.time()
    log("==== Mapas v48 (estilo v47) | QGIS %s | partes: %s | dpi: %s" % (
        Qgis.QGIS_VERSION, ", ".join(sorted(PARTES)), DPI))
    for parte, funcao in (("taxas", rodar_taxas), ("lisa", rodar_lisa), ("gwr", rodar_gwr)):
        if parte in PARTES:
            try:
                funcao()
            except Exception:
                log("ERRO em %s:\n%s" % (parte, traceback.format_exc()))
    log("==== fim em %.0f s" % (time.time() - t0))


principal()
