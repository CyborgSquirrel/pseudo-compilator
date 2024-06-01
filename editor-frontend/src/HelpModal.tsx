import { Modal as BaseModal } from '@mui/base/Modal';
import { Backdrop, FormControlLabel, styled } from '@mui/material';
import Switch from '@mui/material/Switch';

interface HelpModalProps {
  open: boolean,
  setOpen: React.Dispatch<React.SetStateAction<boolean>>,
}

const HelpModal= ({
  open, setOpen,
}: HelpModalProps) => {
  const baccalaureatePseudocode =
`scrie "x = "
citește x
i ← 2
┌cât timp i*i ≤ x execută
│	┌dacă x % i = 0 atunci
│	│	scrie i
│	│	┌dacă i ≠ x/i atunci
│	│	│	scrie x/i
│	│	└■
│	└■
│	i ← i+1
└■
`;

  const myPseudocode =
`scrie "x = "
citește x
i <- 2
cât timp i*i <= x execută
	dacă x % i = 0 atunci
		scrie i
		dacă i != x/i atunci
			scrie x/i
	i <- i+1
`;

  const style: React.CSSProperties = {
    backgroundColor: "darkgray",
    padding: "8px",
    margin: "4px",
    tabSize: "4",
    borderRadius: "8px",
  };

  return (
    <div>
      <Modal
        open={open}
        onClose={() => {
          setOpen(false);
        }}
        aria-labelledby="unstyled-modal-title"
        aria-describedby="unstyled-modal-description"
        slots={{ backdrop: StyledBackdrop }}
      >
        <ModalContent>
          <h2 className="modal-title">
            Compilator de Pseudocod
          </h2>

          <p>
            Această aplicație este un editor și compilator pentru limbajul Pseudocod, care se predă în liceu, la materia de informatică. Varianta limbajului pe care o acceptă compilatorul este una puțin schimbată.
          </p>

          <p>
            Mai jos este un program de Pseudocod, scris în două feluri: varianta din stânga este așa cum apare la examenul de bacalaureat, iar varianta din dreapta este cea acceptată de acest compilator.
          </p>

          <div
            style={{
              display: "flex",
              flexDirection: "row",
            }}
          >
            <pre style={style}>{baccalaureatePseudocode}</pre>
            <pre style={style}>{myPseudocode}</pre>
          </div>

          <p>
            În varianta noastră, blocurile de cod sunt delimitate doar prin indentare. Ca să indentați un bloc de cod, apăsați tasta <kbd>Tab</kbd>.
          </p>

          <p>
            În varianta noastră, anumite caractere care sunt dificile de tipărit, de exemplu: <code>←</code>, <code>≤</code>, sau <code>≠</code> sunt înlocuite cu alternative care se tipăresc mai ușor: <code>&lt;-</code>,  <code>&lt;=</code>, și <code>!=</code>, respectiv.
          </p>

          <p>
            Pentru a închide această fereastră, apăsați tasta <kbd>Esc</kbd>.
          </p>
        </ModalContent>
      </Modal>
    </div>
  )
}

export default HelpModal;

const Modal = styled(BaseModal)`
  position: fixed;
  z-index: 1300;
  inset: 0;
  display: flex;
  align-items: center;
  justify-content: center;
`;

const StyledBackdrop = styled(Backdrop)`
  z-index: -1;
  position: fixed;
  inset: 0;
  background-color: rgb(0 0 0 / 0.5);
  -webkit-tap-highlight-color: transparent;
`;

const ModalContent = styled('div')(
  ({ theme }) => `
    text-align: start;
    position: relative;
    display: flex;
    flex-direction: column;
    gap: 8px;
    overflow: hidden;
    background-color: lightgrey;
    border-color: black;
    border-radius: 8px;
    border-style: solid;
    border-width: 2px;
    padding: 24px;
    color: black;
    max-width: 60ch;

    & .modal-title {
      margin: 0;
    }
  `,
);
