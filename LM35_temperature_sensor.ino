// I connected the middle VOUT pin of the LM35 temperature sensor to
// the A0 port on the Arduino.
int lmPin = A0; 

// Called to perform one-time initialization.
void setup() {
  // Opens the serial port. Need to select same speed in the code and
  // the Arduino -> Tools -> Serial Monitor.
  Serial.begin(9600);
}

float tempC() {
  // analogRead returns an integer value from 0 (low voltage, 0V) to
  // 1023 (high voltage, 5V).
  float raw = analogRead(lmPin);
  float volts = 5.0 * raw / 1023.0;
  // From http://www.ti.com/lit/ds/symlink/lm35.pdf section 7.3.1 we
  // have V_out = (10 mv/°F)(T) (TDH is the degrees F a typo in the
  // lm35 data sheet?) where V_out is the LM35 output voltage and T is
  // the temperature in degrees celsius.
  return volts * 100.0;
}

void loop() {
  Serial.println(tempC());
  delay(200);//ms
}

