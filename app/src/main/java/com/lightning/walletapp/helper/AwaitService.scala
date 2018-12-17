package com.lightning.walletapp.helper

import android.app.{PendingIntent, Service}
import android.support.v4.app.NotificationCompat
import com.lightning.walletapp.MainActivity
import com.lightning.walletapp.R
import android.content.Intent


object AwaitService {
  val reference = classOf[AwaitService]
  val AWAITED_AMOUNT = "inputAmount"
  val CHANNEL_ID = "awaitChannelId"
  val CANCEL = "awaitCancel"
}

class AwaitService extends Service { me =>
  override def onBind(intent: Intent) = null
  override def onStartCommand(intent: Intent, flags: Int, id: Int) = {
    if (intent.getAction == AwaitService.CANCEL) stopNotification else {
      val pendingActivity = PendingIntent.getActivity(me, 0, new Intent(me, MainActivity.wallet), 0)
      val cancelIntent = new Intent(me, AwaitService.reference).setAction(AwaitService.CANCEL)
      val pendingCancelIntent = PendingIntent.getService(me, 0, cancelIntent, 0)
      val awaitedPaymentSum = intent getStringExtra AwaitService.AWAITED_AMOUNT

      val bld = new NotificationCompat.Builder(me, AwaitService.CHANNEL_ID).setContentIntent(pendingActivity)
        .addAction(android.R.drawable.ic_menu_close_clear_cancel, getResources getString R.string.dialog_cancel, pendingCancelIntent)
        .setSmallIcon(R.drawable.ic_info_outline_white_18dp).setContentTitle(getResources getString R.string.notify_title)
        .setContentText(getResources getString R.string.notify_body format awaitedPaymentSum).build

      startForeground(1, bld)
    }

    // Don't recreate if killed
    Service.START_NOT_STICKY
  }

  def stopNotification = {
    me stopForeground true
    stopSelf
  }
}