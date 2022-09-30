import pytorch_lightning as pl
import torch
import segmentation_models_pytorch as smp
import pytorch_lightning as pl
import numpy as np
import rasterio as rio

class CloudModel(pl.LightningModule):
    def __init__(self, weights_path=None, in_channels=13, num_classes=4):
        super().__init__()
        self.model = smp.Unet(
            encoder_name="mobilenet_v2",
            encoder_weights=weights_path,
            in_channels=in_channels,
            classes=num_classes,
        )
    def forward(self, x):
        return self.model(x)
      
def generate_cloud_prob(file, pretrained):
    # Create a model instance
    model = CloudModel()
    model = model.load_from_checkpoint(pretrained)
    model.eval()
    with rio.open(file) as src:
        X = src.read()/10000 # B4, B3, B2
        X = np.pad(X, ((0, 0), (1, 2), (1, 2)), "reflect")
        X = torch.Tensor(X[None, :, :, :]).float()
    preds = model.forward(X)
    preds = torch.argmax(preds, axis=1).squeeze().cpu().detach().numpy()
    # remove padding
    cloud_prob = preds[1:-2, 1:-2]
    return cloud_prob