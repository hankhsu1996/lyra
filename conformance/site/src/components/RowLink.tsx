import { Table } from '@mantine/core';
import type { ReactNode } from 'react';
import { useNavigate } from 'react-router-dom';

// A table row that is one link: pressing anywhere on it opens `to`. The link a
// keyboard or screen reader reaches is the row's first cell, which the caller
// writes as a real link; the rest of the row only answers the pointer.
export function RowLink({ to, children }: { to: string; children: ReactNode }) {
  const navigate = useNavigate();
  return (
    <Table.Tr
      className="row-link"
      onClick={(event) => {
        if (!(event.target instanceof Element) || !event.target.closest('a')) {
          navigate(to);
        }
      }}
    >
      {children}
    </Table.Tr>
  );
}
