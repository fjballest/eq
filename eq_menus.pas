uses
  TPString,
  TPCrt,
  TPMenu;

procedure InitMenu(var M : Menu);
const
  Color1 : MenuColorArray = ($09, $09, $03, $1E, $0B, $0E);
  Color2 : MenuColorArray = ($06, $2E, $03, $1E, $0B, $0E);
  Color3 : MenuColorArray = ($04, $4E, $03, $1E, $0B, $0E);
  Color4 : MenuColorArray = ($04, $2E, $03, $1E, $0B, $0E);
  Frame1 : FrameArray = 'ÉÈ»¼Íº';
  Frame2 : FrameArray = 'ÕÔ¸¾Í³';

begin
  {Customize this call for special exit characters and custom item displays}
  M := NewMenu([], nil);

  SubMenu(1,3,1,Horizontal,Frame1,Color1,'Menu Principal');
    MenuItem('Acidos',2,1,101,'Elegir un  cido para la valoraci¢n.');
    SubMenu(3,5,1,Vertical,Frame1,Color2,'');
      MenuItem('ClH',1,1,201,'Acido Clorh¡drico [Fuerte]');
      MenuItem('CH3COOH',2,1,202,'Acido Ac‚tico [D‚bil]');
      PopSublevel;
    MenuItem('Bases',14,1,102,'Elegir una base para la valoraci¢n.');
    SubMenu(15,5,1,Vertical,Frame1,Color2,'');
      MenuItem('NaOH',1,1,301,'Hidr¢xido S¢dico [Fuerte]');
      MenuItem('NH3',2,1,302,'Amoniaco [D‚bil]');
      PopSublevel;
    MenuItem('Tipo de Valoraci¢n',24,1,103,'Seleccionar a qui‚n se valora.');
    SubMenu(28,5,1,Vertical,Frame1,Color2,'');
      MenuItem('Acido con Base',1,1,401,'Valorar el  cido con la base.');
      MenuItem('Base con Acido',2,1,402,'Valorar la base con el  cido.');
      PopSublevel;
    MenuItem('Opciones',47,1,104,'Cambiar opciones para la simulaci¢n');
    SubMenu(48,5,1,Vertical,Frame1,Color2,'');
      MenuItem('Salida a Impresora',1,10,501,'Imprimir o no la tabla de valoraci¢n.');
      MenuItem('Salida a fichero',2,10,502,'Copiar la tabla de valoraci¢n a un fichero');
      MenuItem('M‚todos de valoraci¢n',3,1,503,'Elegir los m‚todos gr ficos para la valoraci¢n');
      SubMenu(50,9,1,Vertical,Frame2,Color3,'Dibujar');
        MenuItem('Curva de valoraci¢n',1,1,701,'Dibujar o no la curva de valoraci¢n.');
        MenuItem('Primera derivada',2,1,702,'Dibujar o no la primera derivada de la curva.');
        MenuItem('Tangentes',3,1,703,'Dibujar o no el m‚todo de las tangentes.');
        MenuItem('Indicador',4,1,704,'Dibujar o no el area de aplicaci¢n del indicador.');
        MenuItem('Dibujo del Montaje',5,1,705,'Grafica del dispositivo de laboratorio');
        PopSublevel;
      MenuItem('Precisi¢n',4,1,504,'Elegir precisi¢n para la simulaci¢n');
      SubMenu(60,9,1,Vertical,Frame1,Color4,'');
        MenuItem('Normal',1,1,801,'Precisi¢n usual para simulaci¢n manual');
        MenuItem('Maxima',2,1,802,'Utilizar la m xima precisi¢n disponible');
        PopSublevel;
      MenuItem('Configuraci¢n',5,1,505,'Elegir opiones del entorno');
      SubMenu(53,10,1,Vertical,Frame1,Color3,'Entorno');
        MenuItem('Explotar ventanas',1,1,901,'Explotar ventanas al crearlas/cerrarlas o no hacerlo');
        MenuItem('Sonido',2,1,902,'Activar/Desactivar sonido');
        PopSublevel;
      PopSublevel;
    MenuItem('Valorar',60,1,105,'Simular la valoraci¢n.');
    MenuItem('Salir',73,1,106,'Terminar la ejecuci¢n del programa.');
    PopSublevel;

  ResetMenu(M);
end;

begin
end.
