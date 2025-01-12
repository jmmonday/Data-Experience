/********************************************************
  Check Sippers
********************************************************/
void  CheckSippers() {
  ReadBatteryLevel();  //Read battery level
  sleeptimerfunction ();  // Is it time to log data? (Edit variable "logfreq" in a_header to change this timing)

  // Monitor Left Sip
  leftmillis = millis();

  // Monitor Right Sip
  rightmillis = millis();


  // Show time elapsed when C is pressed
  while (digitalRead (buttonC) == LOW) {
    DisplayTimeElapsed();
  }

  // Toggle between screen being off with Button B
  if (digitalRead (buttonB) == LOW) {
    if (SleepDisplay == true) {
      SleepDisplay = false;
      display.clearDisplay();
      display.setCursor(10, 10);
      display.println("Display ON");
      display.display();
      Blink (RED_LED, 50, 2);
      delay (500);
    }

    else {
      SleepDisplay = true;
      display.clearDisplay();
      display.setCursor(10, 10);
      display.println("Display OFF");
      display.display();
      Blink (RED_LED, 50, 2);
      delay (500);
    }
  }
}
