import { Modal as BaseModal } from '@mui/base/Modal';
import { Backdrop, FormControlLabel, styled } from '@mui/material';
import Switch from '@mui/material/Switch';

interface SettingsModalProps {
  open: boolean,
  setOpen: React.Dispatch<React.SetStateAction<boolean>>,
  compileLists: boolean,
  setCompileLists: React.Dispatch<React.SetStateAction<boolean>>,
}

const SettingsModal = ({
  open, setOpen,
  compileLists, setCompileLists,
}: SettingsModalProps) => {
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
            Setări
          </h2>
          <FormControlLabel
            label="permite compilatorului să compileze liste"
            control={
              <Switch
                checked={compileLists}
                onChange={(event) => {
                  setCompileLists(event.target.checked);
                }}
              />
            }
          />
        </ModalContent>
      </Modal>
    </div>
  )
}

export default SettingsModal;

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

    & .modal-title {
      margin: 0;
    }
  `,
);
