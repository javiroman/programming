import * as React from 'react';

import {
  Button,
  EmptyState,
  EmptyStateIcon,
  EmptyStateVariant,
  PageSection,
  Title,
  Progress, ProgressSize, ProgressMeasureLocation, ProgressVariant
} from '@patternfly/react-core';
import {CubesIcon} from "@patternfly/react-icons";


const Dashboard: React.FunctionComponent = () => {
  const [currentValue, setCurrentValue] = React.useState(0);

  function sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
  }

  const handleStream = () => {
    const eventSource = new EventSource(`http://localhost:8080/hello/stream/11/step`);
    eventSource.onmessage = (e) => {
      let cadena = e.data;
      let increment = parseInt(cadena.split("-")[1])*10;
      console.log(e.data);
      if (increment == 100) {
        eventSource.close();
      }
      setCurrentValue(increment);
    }
 }

  const handleClick = () => {
    handleStream();
  };

  const onProgressUpdate = nextValue => {
    setCurrentValue(nextValue);
  };

  return (
    <PageSection>
      <EmptyState variant={EmptyStateVariant.full}>
        <EmptyStateIcon icon={CubesIcon} />
        <Title headingLevel="h1" size="lg">
          Progress Bar Example
        </Title>
        <Button onClick={handleClick} variant="primary">
          Execute Action
        </Button>
        <br/>
        <br/>
        <br/>
        <Progress value={currentValue} size={ProgressSize.lg} measureLocation={ProgressMeasureLocation.outside} />
      </EmptyState>
    </PageSection>
    )
}

export { Dashboard };
